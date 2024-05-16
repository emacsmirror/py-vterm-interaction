;;; python-vterm.el --- A mode for Python REPL using vterm -*- lexical-binding: t -*-

;; Copyright (C) 2020-2023 Shigeaki Nishina

;; Author: Shigeaki Nishina, Valentin Boettcher
;; Maintainer: Valentin Boettcher <hiro at protagon.space>
;; Created: May 11, 2024
;; URL: https://github.com/vale981/python-vterm.el
;; Package-Requires: ((emacs "25.1") (vterm "0.0.1"))
;; Version: 1.0.3
;; Keywords: languages, python

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses/.

;;; Commentary:

;; Provides a major-mode for inferior Python process that runs in vterm, and a
;; minor-mode that extends python-mode to support interaction with the inferior
;; Python process.  This is primarily useful for running fancy python shells like
;; ipython or ptpython.
;; This is a straight port of julia-vterm.el by Shigeaki Nishina
;; (https://github.com/shg/julia-vterm.el) with only minor modifications.

;;; Usage:

;; You must have python-mode and vterm installed.
;; Install python-vterm.el manually using package.el
;;
;;   (package-install-file "/path-to-download-dir/python-vterm.el")
;;
;; Eval the following line. Add this line to your init file to enable this
;; mode in future sessions.
;;
;;   (add-hook 'python-mode-hook #'python-vterm-mode)
;;
;; Now you can interact with an inferior Python REPL from a Python buffer.
;;
;; C-c C-z in a python-mode buffer to open an inferior Python REPL buffer.
;; C-c C-z in the REPL buffer to switch back to the script buffer.
;; C-c C-c in the script buffer to send region or current line to REPL.
;;
;; See the code below for a few more key bidindings.

;;; Code:

(require 'vterm)
(require 'rx)


;;----------------------------------------------------------------------
(defgroup python-vterm-repl nil
  "A major mode for inferior Python REPL."
  :group 'python)

(defvar-local python-vterm-repl-program "python"
  "Name of the command for executing Python code.
Maybe either a command in the path, like python
or an absolute path name, like /usr/local/bin/python
parameters may be used, like python -q")

(defvar-local python-vterm-silent-cells nil
  "If non-nil, the PYTHON-VTERM-SEND-CURRENT-CELL will use ipythons `%run` magic to run a code cell.

For plain python `exec(open(...).read())` is used.")

(defvar-local python-vterm-repl-script-buffer nil)
(defvar-local python-vterm-repl-interpreter :python
  "Reflects whether the inferior Python REPL is a Python or IPython shell.

If in doubt, set this to :python.")

(defvar python-vterm-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-z") #'python-vterm-repl-switch-to-script-buffer)
    (define-key map (kbd "M-k") #'python-vterm-repl-clear-buffer)
    (define-key map (kbd "C-c C-t") #'python-vterm-repl-copy-mode)
    (define-key map (kbd "C-l") #'recenter-top-bottom)
    (define-key map (kbd "C-c M-r") #'python-vterm-repl-restart)
    map))

(define-derived-mode python-vterm-repl-mode vterm-mode "Inf-Python"
  "A major mode for inferior Python REPL."
  :group 'python-vterm-repl)

(defcustom python-vterm-repl-mode-hook nil
  "Hook run after starting a Python script buffer with an inferior Python REPL."
  :type 'hook
  :group 'python-vterm-repl)

(defun python-vterm-repl-buffer-name (&optional session-name)
  "Return a Python REPL buffer name whose session name is SESSION-NAME.
If SESSION-NAME is not given, the default session name `main' is assumed."
  (format "*python:%s*" (or session-name "main")))

(defun python-vterm-repl-session-name (repl-buffer)
  "Return the session name of REPL-BUFFER."
  (let ((bn (buffer-name repl-buffer)))
    (if (string= (substring bn 1 8) "python:")
        (substring bn 8 -1)
      nil)))


(defun python-vterm--launch (ses-name env context)
  "Launch a new Python REPL buffer with SES-NAME and ENV.

If CONTEXT is given, it is used to set the working directory and the script buffer."
  (let ((new-buffer
         (generate-new-buffer (python-vterm-repl-buffer-name ses-name)))
        (vterm-shell python-vterm-repl-program)
        (vterm-environment (if context (plist-get context :env) env)))
    (with-current-buffer new-buffer
      (when context
        (setq default-directory (plist-get context :cwd))
        (setq python-vterm-repl-script-buffer (plist-get context :script-buffer)))
      (python-vterm-repl-mode)
      (run-with-timer .5 nil
                      (lambda (buffer)
                        (with-current-buffer buffer
                          (while (not (python-vterm-repl-prompt-status))
                            (sit-for 0.1))
                          (setq python-vterm-repl-interpreter
                                (if (eq (python-vterm--execute-script "is_ipython") :false)
                                    :python :ipython))))
                      new-buffer)

      (add-function :filter-args (process-filter vterm--process)
                    (python-vterm-repl-run-filter-functions-func ses-name)))
    new-buffer))

(defun python-vterm-repl-buffer (&optional session-name restart)
  "Return an inferior Python REPL buffer of the session name SESSION-NAME.
If there exists no such buffer, one is created and returned.
With non-nil RESTART, the existing buffer will be killed and
recreated."
  (let ((ses-name (or session-name "main"))
        (env (cons "TERM=xterm-256color" process-environment)))
    (if-let ((buffer (get-buffer (python-vterm-repl-buffer-name ses-name)))
             (alive (vterm-check-proc buffer))
             (no-restart (not restart)))
        buffer
      (if (not buffer)
          (python-vterm--launch ses-name env nil)
        (save-excursion
          (let* ((win (get-buffer-window buffer))
                 (proc (get-buffer-process buffer))
                 (context (if proc (python-vterm-repl-get-context buffer))))
            (with-current-buffer buffer
              (rename-buffer (concat (buffer-name) ":orphaned")))
            (let ((new-buffer (python-vterm--launch ses-name env context)))
              (when win
                (select-window win)
                (switch-to-buffer new-buffer))
              (if (process-live-p proc) (delete-process proc))
              new-buffer)))))))

(defun python-vterm-repl-list-sessions ()
  "Return a list of existing Python REPL sessions."
  (mapcan (lambda (bn)
            (if (string-match "\\*python:\\(.*\\)\\*" bn)
                (list (match-string 1 bn))
              nil))
          (mapcar #'buffer-name (buffer-list))))

(defun python-vterm-repl (&optional arg)
  "Create an inferior Python REPL buffer and open it.
The buffer name will be `*python:main*' where `main' is the default session name.
With prefix ARG, prompt for a session name.
If there's already an alive REPL buffer for the session, it will be opened."
  (interactive "P")
  (let* ((session-name
          (cond ((null arg) nil)
                (t (completing-read "Session name: " (python-vterm-repl-list-sessions) nil nil nil nil
                                    (python-vterm-repl-session-name (python-vterm-fellow-repl-buffer))))))
         (orig-buffer (current-buffer))
         (repl-buffer (python-vterm-repl-buffer session-name)))
    (if (and (boundp 'python-vterm-mode) python-vterm-mode)
        (with-current-buffer repl-buffer
          (setq python-vterm-repl-script-buffer orig-buffer)))
    (pop-to-buffer-same-window repl-buffer)))

(defun python-vterm-repl-switch-to-script-buffer ()
  "Switch to the script buffer that is paired with this Python REPL buffer."
  (interactive)
  (let ((repl-buffer (current-buffer))
        (script-buffer (if (buffer-live-p python-vterm-repl-script-buffer)
                           python-vterm-repl-script-buffer
                         nil)))
    (if script-buffer
        (with-current-buffer script-buffer
          (setq python-vterm-fellow-repl-buffer repl-buffer)
          (switch-to-buffer-other-window script-buffer)))))

(defun python-vterm-repl-restart ()
  "Restart the inferior Python process in the current REPL buffer."
  (interactive)
  (if (y-or-n-p "Restart Python REPL? ")
      (python-vterm-repl-buffer (python-vterm-repl-session-name (current-buffer)) t)))

(defun python-vterm-repl-clear-buffer ()
  "Clear the content of the Python REPL buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (vterm-clear 1)))

(defvar-local python-vterm-repl-filter-functions '()
  "List of filter functions that process the output to the REPL buffer.")

(defun python-vterm-repl-run-filter-functions-func (session)
  "Return a function that runs registered filter functions for SESSION with args."
  (lambda (args)
    (with-current-buffer (python-vterm-repl-buffer session)
      (let ((proc (car args))
            (str (cadr args)))
        (let ((funcs python-vterm-repl-filter-functions))
          (while funcs
            (setq str (apply (pop funcs) (list str))))
          (list proc str))))))

(defun python-vterm-repl-prompt-status ()
  "Check and return the prompt status of the REPL.
Return a corresponding symbol or nil if not ready for input."
  (let* ((bs (buffer-string))
         (tail (substring bs (- (min 256 (length bs))))))
    (set-text-properties 0 (length tail) nil tail)
    (let* ((lines (split-string (string-trim-right
                                 (replace-regexp-in-string
                                  (rx (1+ bol (0+ whitespace) eol))
                                  "" tail)
                                 "[\t\n\r]+")
                                (char-to-string ?\n)))
           (prompt (car (last lines))))
      (pcase prompt
        ((rx bol ">>> " eol) :python)
        ((rx bol "In [" (one-or-more (any "0-9")) "]: " eol) :ipython)))))


(defun python-vterm--get-script-file (name)
  "Return the full path of the script file with NAME."
  (expand-file-name (concat "./scripts/" name ".py")
                    (file-name-directory (symbol-file 'python-vterm-mode))))

(defun python-vterm--execute-script (name &rest args)
  "Load the script with file NAME and call the eponymous function with ARGS.

The script file is expected to be in the `scripts' directory of
the package and must contain exactly one function with the same
name as the file without the extension.  The function must return
a JSON-serializable object.  The function is called with the
arguments ARGS and the result is returned as a parsed JSON object
in the plist format.  The functions from the utility script are
loaded into the repl as well.  All loaded functions and modules
will be cleaned up afterwards."
  (let ((utility-file (python-vterm--get-script-file name))
        (script-file (python-vterm--get-script-file "utility"))
        (tmpfile (make-temp-file "python-vterm--" nil ".json"))
        (python-vterm-paste-with-return nil)
        (python-vterm-paste-with-clear nil)
        (arglist (concat
                  (seq-reduce
                   (lambda (el rest)
                     (format "%s, \"%s" el rest))
                   args "")
                  (if (seq-empty-p args) "" "\"")))
        (up-to-now (buffer-string))
        (result nil))

    (python-vterm-clear-line)
    (python-vterm-paste-string (format "exec(open(\"%s\").read());" utility-file))
    (python-vterm-paste-string (format "exec(open(\"%s\").read());" script-file))
    (python-vterm-paste-string (format "%s(\"%s\"%s);" name tmpfile arglist))

    ;; clean up all utility stuff
    (python-vterm-paste-string "del dump_json;")
    (python-vterm-paste-string (format "del %s" name))
    (python-vterm-send-return-key)

    (while
        (progn (with-temp-buffer
                 (insert-file-contents tmpfile)
                 (buffer-string)
                 (if (= (buffer-size) 0)
                     (progn
                       (sleep-for 0.1)
                       t)
                   (progn
                     (delete-file tmpfile)
                     (setq result (json-parse-buffer :object-type 'plist :array-type 'list))
                     nil)))))
    (let ((inhibit-read-only t))
      (python-vterm-repl-clear-buffer)
      (insert up-to-now))
    result))


(defun python-vterm--read-script (name)
  "Read the content of the script file with NAME.
The path of the script is expanded relative to the `scripts' directory."
  (let ((script-file (python-vterm--get-script-file name)))
    (with-temp-buffer
      (insert-file-contents script-file)
      (buffer-string))))

(defun python-vterm-repl-get-context (buf)
  "Obtain context information of the REPL buffer BUF.
This returns a list of the current working directory of teh
inferior Python process and the current active environment."
  (with-current-buffer buf
    (let ((context (python-vterm--execute-script "get_env")))
      (plist-put context :script-buffer python-vterm-repl-script-buffer))))

(defvar python-vterm-repl-copy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") #'python-vterm-repl-copy-mode)
    (define-key map [return] #'python-vterm-repl-copy-mode-done)
    (define-key map (kbd "RET") #'python-vterm-repl-copy-mode-done)
    (define-key map (kbd "C-c C-r") #'vterm-reset-cursor-point)
    map))

(define-minor-mode python-vterm-repl-copy-mode
  "Toggle copy mode."
  :group 'python-vterm-repl
  :lighter " VTermCopy"
  :keymap python-vterm-repl-copy-mode-map
  (if python-vterm-repl-copy-mode
      (progn
        (message "Start copy mode")
        (use-local-map nil)
        (vterm-send-stop))
    (vterm-reset-cursor-point)
    (use-local-map python-vterm-repl-mode-map)
    (vterm-send-start)
    (message "End copy mode")))

(defun python-vterm-repl-copy-mode-done ()
  "Save the active region to the kill ring and exit copy mode."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (user-error "No active region"))
  (python-vterm-repl-copy-mode -1))


;;----------------------------------------------------------------------
(defgroup python-vterm nil
  "A minor mode to interact with an inferior Python REPL."
  :group 'python)

(defcustom python-vterm-hook nil
  "Hook run after starting a Python script buffer with an inferior Python REPL."
  :type 'hook
  :group 'python-vterm)

(defvar-local python-vterm-paste-with-return t
  "Whether to send a return key after pasting a string to the Python REPL.")

(defvar-local python-vterm-paste-with-clear t
  "Whether to clear the line before pasting a string to the Python REPL.")

(defvar-local python-vterm-fellow-repl-buffer nil)
(defvar-local python-vterm-session nil)
(defvar-local python-vterm-context nil)

(defun python-vterm-fellow-repl-buffer (&optional session-name)
  "Return the paired REPL buffer or the one specified with SESSION-NAME."
  (if session-name
      (python-vterm-repl-buffer session-name)
    (if (buffer-live-p python-vterm-fellow-repl-buffer)
        python-vterm-fellow-repl-buffer
      (if python-vterm-session
          (python-vterm-repl-buffer python-vterm-session)
        (python-vterm-repl-buffer)))))

(defun python-vterm-switch-to-repl-buffer (&optional arg)
  "Switch to the paired REPL buffer or to the one with a specified session name.
With prefix ARG, prompt for session name."
  (interactive "P")
  (let* ((session-name
          (cond ((null arg) nil)
                (t (completing-read "Session name: " (python-vterm-repl-list-sessions) nil nil nil nil
                                    (python-vterm-repl-session-name (python-vterm-fellow-repl-buffer))))))
         (script-buffer (current-buffer))
         (repl-buffer (python-vterm-fellow-repl-buffer session-name)))
    (setq python-vterm-fellow-repl-buffer repl-buffer)
    (with-current-buffer repl-buffer
      (setq python-vterm-repl-script-buffer script-buffer)
      (switch-to-buffer-other-window repl-buffer))))

(defun python-vterm-send-return-key ()
  "Send a return key to the Python REPL."
  (with-current-buffer (python-vterm-fellow-repl-buffer)
    (vterm-send-return)))

(defun python-vterm-send-backspace ()
  "Send a backspace key to the Python REPL."
  (with-current-buffer (python-vterm-fellow-repl-buffer)
    (vterm-send-backspace)))

(defun python-vterm-send-current-line ()
  "Send the current line to the Python REPL, and move to the next line.
This sends a newline after the content of the current line even if there's no
newline at the end.  A newline is also inserted after the current line of the
script buffer."
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((clmn (current-column))
          (char (char-after))
          (line (string-trim (thing-at-point 'line t))))
      (unless (and (zerop clmn) char)
        (when (/= 0 clmn)
          (python-vterm-paste-string line)
          (if (not python-vterm-paste-with-return)
              (python-vterm-send-return-key))
          (if (not char)
              (newline))))))
  (forward-line))

(defun python-vterm-paste-string (string &optional session-name)
  "Send STRING to the Python REPL buffer using brackted paste mode.
If SESSION-NAME is given, the REPL with the session name, otherwise
the main REPL, is used."
  (with-current-buffer (python-vterm-fellow-repl-buffer session-name)
    (goto-char (point-max))
    (when python-vterm-paste-with-clear
      (python-vterm-clear-line session-name))
    (vterm-send-string string t)
    (if python-vterm-paste-with-return
        (python-vterm-send-return-key))))

(defun python-vterm-clear-line (&optional session-name)
  "Clear the current line in the Python REPL buffer."
  (with-current-buffer (python-vterm-fellow-repl-buffer session-name)
    (goto-char (point-max))
    (vterm-send-key (kbd "C-a"))
    (vterm-send-key (kbd "C-k"))))

(defun python-vterm-ensure-newline (str)
  "Add a newline at the end of STR if the last character is not a newline."
  (concat str (if (string= (substring str -1 nil) "\n") "" "\n")))

(defun python-vterm-send-region-or-current-line ()
  "Send the content of the region if the region is active, or send the current line."
  (interactive)
  (if (use-region-p)
      (let ((str (buffer-substring-no-properties (region-beginning) (region-end))))
        (python-vterm-paste-string str)
        (deactivate-mark))
    (python-vterm-send-current-line)))

(defun python-vterm--load-file (file &optional comment)
  "Load the content of the file with FILE into the Python REPL buffer."
  (if (eq python-vterm-repl-interpreter :ipython)
      (format "%%run -i %s # %s" file (or comment ""))
    (format "exec(open(\"%s\").read()) # %s" file comment)))

(defun python-vterm--send-maybe-silent (string &optional comment)
  "Send STRING to the Python REPL buffer, possibly using `%run -i' with a temp file."

  (with-current-buffer (python-vterm-fellow-repl-buffer)
    (if python-vterm-silent-cells
        (let ((tmpfile (make-temp-file "python-vterm" nil ".py")))
          (with-temp-file tmpfile
            (insert string))
          (python-vterm-paste-string (python-vterm--load-file tmpfile comment))
          (run-with-timer 10 nil
                          (lambda (tmpfile)
                            (delete-file tmpfile))
                          tmpfile))
      (python-vterm-paste-string string))))

(defun python-vterm-send-current-cell ()
  "Send the current code \"cell\" to the Python REPL.
Each block is delimited by `# %% <optional name>`.

If no marker is present before the point, the cell is assumed to
begin with the buffer. Likewise, if there is no marker after the
point, the cell is assumed to end with the buffer."
  (interactive)
  (let ((cell-regex "^# %%[ \t]*\\(.*\\)?"))
    (save-excursion
      (end-of-line)
      (let* ((start (or (save-excursion (re-search-backward cell-regex (point-min) t))
                        (point-min)))
             (cell-name (if (= start (point-min))
                            "(header)"
                          (match-string 1)))
             (end (or (save-excursion (re-search-forward cell-regex (point-max) t))
                      (point-max)))
             (cell-content (buffer-substring-no-properties start end)))
        (python-vterm--send-maybe-silent cell-content cell-name)
        (when (not python-vterm-paste-with-return)
          (python-vterm-send-return-key))))))

(defun python-vterm-run-current-function ()
  "Send the current function the Python REPL and paste its name, ready to run.
If the function has no arguments, the function call is run immediately."
  (interactive)
  (let* ((function-name-regex "def[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]*(\\(.*\\)):"))
    (save-mark-and-excursion
      (python-mark-defun)
      (let ((name-found (save-mark-and-excursion (re-search-forward function-name-regex (mark) t))))
        (if name-found
            (let ((name (match-string 1))
                  (args (match-string 2))
                  (python-vterm-paste-with-return nil))
              (let ((func (buffer-substring-no-properties (region-beginning) (region-end))))
                (python-vterm--send-maybe-silent func name)
                (python-vterm-send-return-key))

              (if (string-empty-p args)
                  (progn
                    (python-vterm-paste-string (format "%s()" name))
                    (python-vterm-send-return-key))
                (python-vterm-paste-string (format "%s(" name))))
          (message "No function found"))))))

(defun python-vterm-send-buffer ()
  "Send the whole content of the script buffer to the Python REPL line by line."
  (interactive)
  (save-excursion
    (python-vterm-paste-string (python-vterm-ensure-newline (buffer-string)))))

(defun python-vterm-send-run-buffer-file ()
  "Run the current buffer file in the python vterm buffer.

  This is equivalent to running `%run -i <buffer-file-name>` in the python vterm buffer."
  (interactive)
  (let ((file buffer-file-name))
    (with-current-buffer (python-vterm-fellow-repl-buffer)
      (python-vterm-paste-string (python-vterm--load-file file "load script buffer")))))

(defun python-vterm-send-cd-to-buffer-directory ()
  "Change the REPL's working directory to the directory of the buffer file."
  (interactive)
  (if buffer-file-name
      (let ((buffer-directory (file-name-directory buffer-file-name)))
        (with-current-buffer (python-vterm-fellow-repl-buffer)
          (python-vterm-paste-string (if (eq python-vterm-repl-interpreter :ipython)
                                         (format "%%cd %s" buffer-directory)
                                       (format "import os; os.chdir(\"%s\")" buffer-directory)))
          (setq default-directory buffer-directory)))
    (message "The buffer is not associated with a directory.")))

(defalias 'python-vterm-sync-wd 'python-vterm-send-cd-to-buffer-directory)

(defun python-vterm-fellow-repl-prompt-status ()
  "Return REPL mode or nil if REPL is not ready for input."
  (with-current-buffer (python-vterm-fellow-repl-buffer)
    (python-vterm-repl-prompt-status)))

;;;###autoload
(define-minor-mode python-vterm-mode
  "A minor mode for a Python script buffer that interacts with an inferior Python REPL."
  :init-value nil
  :lighter " PY"
  :keymap
  `((,(kbd "C-c C-z") . python-vterm-switch-to-repl-buffer)
    (,(kbd "C-c C-c") . python-vterm-send-region-or-current-line)
    (,(kbd "C-c C-j") . python-vterm-send-current-cell)
    (,(kbd "C-c C-f") . python-vterm-run-current-function)
    (,(kbd "C-c C-b") . python-vterm-send-buffer)
    (,(kbd "C-c C-r") . python-vterm-send-run-buffer-file)
    (,(kbd "C-c C-d") . python-vterm-send-cd-to-buffer-directory)))


;;----------------------------------------------------------------------
;; Define some utility aliases but not override if the names are already used.
(unless (fboundp 'python)
  (defalias 'python 'python-vterm-repl))

(unless (boundp 'python-session)
  (defvaralias 'python-session 'python-vterm-session))


(provide 'python-vterm)

;;; python-vterm.el ends here
