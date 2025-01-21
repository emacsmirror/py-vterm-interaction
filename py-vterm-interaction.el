;;; py-vterm-interaction.el --- A mode for Python REPL using vterm -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024 Shigeaki Nishina, Valentin Boettcher

;; Author: Shigeaki Nishina, Valentin Boettcher
;; Maintainer: Valentin Boettcher <hiro at protagon.space>
;; Created: May 11, 2024
;; URL: https://github.com/vale981/py-vterm-interaction.el
;; Package-Requires: ((emacs "27.1") (vterm "0.0.2") (python "0.28"))
;; Version: 1.0.6
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

;; Provides a major-mode for inferior Python process that runs in
;; vterm, and a minor-mode that extends python-mode to support
;; interaction with the inferior Python process.  This is primarily
;; useful for running fancy python shells like ipython or ptpython.
;; This is a straight port of julia-vterm.el by Shigeaki Nishina
;; (https://github.com/shg/julia-vterm.el) with some python specific
;; modifications to provide functionality similar to spyder.

;;; Usage:

;; You must have python-mode and vterm installed.
;; Install py-vterm-interaction.el manually using package.el
;;
;;   (package-install-file "/path-to-download-dir/py-vterm-interaction.el")
;;
;; Eval the following line. Add this line to your init file to enable this
;; mode in future sessions.
;;
;;   (add-hook 'python-mode-hook #'py-vterm-interaction-mode)
;;
;; Now you can interact with an inferior Python REPL from a Python buffer.
;;
;; C-c C-z in a python-mode buffer to open an inferior Python REPL buffer
;;         optionally specifying a session name with a prefix argument
;; C-c C-z in the REPL buffer to switch back to the script buffer.
;; C-c C-c in the script buffer to send region or current line to REPL.
;;
;; See the code below for a few more key bidindings.

;;; Code:

(require 'vterm)
(require 'python)
(require 'rx)


;;----------------------------------------------------------------------
(defgroup py-vterm-interaction-repl nil
  "A major mode for inferior Python REPL."
  :group 'python)

(defvar-local py-vterm-interaction-repl-program "python"
  "Name of the command for executing Python code.
Maybe either a command in the path, like python
or an absolute path name, like /usr/local/bin/python
parameters may be used, like python -q")

(defvar-local py-vterm-interaction-silent-cells nil
  "Controls whether to paste code into the repl.
If non-nil, the PYTHON-VTERM-SEND-CURRENT-CELL will use ipythons
`%run` magic to run a code cell.

For plain python `exec(open(...).read())` is used.")

(defvar-local py-vterm-interaction-repl-script-buffer nil)
(defvar-local py-vterm-interaction-repl-interpreter :python
  "Reflects whether the inferior Python REPL is a Python or IPython shell.

If in doubt, set this to :python.")

(defvar py-vterm-interaction-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-z") #'py-vterm-interaction-repl-switch-to-script-buffer)
    (define-key map (kbd "M-k") #'py-vterm-interaction-repl-clear-buffer)
    (define-key map (kbd "C-c C-t") #'py-vterm-interaction-repl-copy-mode)
    (define-key map (kbd "C-l") #'recenter-top-bottom)
    (define-key map (kbd "C-c M-r") #'py-vterm-interaction-repl-restart)
    map))

(define-derived-mode py-vterm-interaction-repl-mode vterm-mode "Inf-Python"
  "A major mode for inferior Python REPL."
  :group 'py-vterm-interaction-repl)

(defcustom py-vterm-interaction-repl-mode-hook nil
  "Hook run after starting a Python script buffer with an inferior Python REPL."
  :type 'hook
  :group 'py-vterm-interaction-repl)

(defcustom py-vterm-interaction-repl-launch-timeout 5
  "Time in seconds to wait for the Python REPL to start."
  :type 'number
  :group 'py-vterm-interaction-repl)

(defcustom py-vterm-interaction-repl-script-timeout 5
  "Time in seconds to wait for the Python REPL to execute a script."
  :type 'number
  :group 'py-vterm-interaction-repl)

(defvar-local py-vterm-interaction-paste-with-return t
  "Whether to send a return key after pasting a string to the Python REPL.")

(defvar-local py-vterm-interaction-paste-with-clear t
  "Whether to clear the line before pasting a string to the Python REPL.")

(defvar-local py-vterm-interaction-fellow-repl-buffer nil
  "The REPL buffer associated with the python buffer.")

(defvar-local py-vterm-interaction-session nil
  "The name of the session associated with a REPL or python buffer.")

(defvar-local py-vterm-interaction-context nil
  "Information about the environment and python session of the REPL buffer.")

(defun py-vterm-interaction-repl-buffer-name (&optional session-name)
  "Return a Python REPL buffer name whose session name is SESSION-NAME.
If SESSION-NAME is not given, the default session name `main' is assumed."
  (format "*python:%s*" (or session-name "main")))

(defun py-vterm-interaction-repl-session-name (repl-buffer)
  "Return the session name of REPL-BUFFER."
  (let ((bn (buffer-name repl-buffer)))
    (if (string= (substring bn 1 8) "python:")
        (substring bn 8 -1)
      nil)))

(defvar py-vterm-interaction-repl--launch-timers '()
  "A timer that is used to determine the interpreter upon launch.")


(defun py-vterm-interaction--launch (ses-name env context)
  "Launch a new Python REPL buffer with SES-NAME and ENV.

If CONTEXT is given, it is used to set the working directory and
the script buffer.  It is also attempted to detect whether the
python interpreter is ipython.  This times out after
`py-vterm-interaction-repl-launch-timeout' seconds."
  (let ((new-buffer
         (generate-new-buffer (py-vterm-interaction-repl-buffer-name ses-name)))
        (vterm-shell py-vterm-interaction-repl-program)
        (vterm-environment (if context (plist-get context :env) env))
        (id (gensym "py-vterm-interaction-buffer")))
    (with-current-buffer new-buffer
      (when context
        (setq default-directory (plist-get context :cwd))
        (setq py-vterm-interaction-repl-script-buffer (plist-get context :script-buffer)))
      (py-vterm-interaction-repl-mode)

      (push (cons id
                  (run-with-timer .1 1
                                  (lambda (buffer)
                                    (let ((timer (alist-get id py-vterm-interaction-repl--launch-timers)))
                                      (if (and buffer (buffer-live-p buffer))
                                          (if (py-vterm-interaction-repl-prompt-status)
                                              (progn
                                                (setq py-vterm-interaction-repl-interpreter
                                                      (if (eq (py-vterm-interaction--execute-script "is_ipython") :false)
                                                          :python :ipython))
                                                (cancel-timer timer)))
                                        (cancel-timer timer))))
                                  new-buffer))
            py-vterm-interaction-repl--launch-timers)
      (add-function :filter-args (process-filter vterm--process)
                    (py-vterm-interaction-repl-run-filter-functions-func ses-name))
      (setq-local py-vterm-interaction-session ses-name))
    new-buffer))

(defun py-vterm-interaction-repl-buffer (&optional session-name restart)
  "Return an inferior Python REPL buffer of the session name SESSION-NAME.
If there exists no such buffer, one is created and returned.
With non-nil RESTART, the existing buffer will be killed and
recreated."
  (let ((ses-name (or session-name py-vterm-interaction-session "main"))
        (env (cons "TERM=xterm-256color" process-environment)))
    (if-let ((buffer (get-buffer (py-vterm-interaction-repl-buffer-name ses-name)))
             (alive (vterm-check-proc buffer))
             (no-restart (not restart)))
        buffer
      (if (not buffer)
          (py-vterm-interaction--launch ses-name env nil)
        (save-excursion
          (let* ((win (get-buffer-window buffer))
                 (proc (get-buffer-process buffer))
                 (context (if proc (py-vterm-interaction-repl-get-context buffer))))
            (with-current-buffer buffer
              (rename-buffer (concat (buffer-name) ":orphaned")))
            (let ((new-buffer (py-vterm-interaction--launch ses-name env context)))
              (when win
                (select-window win)
                (switch-to-buffer new-buffer))
              (if (process-live-p proc) (delete-process proc))
              new-buffer)))))))

(defun py-vterm-interaction-repl-list-sessions ()
  "Return a list of existing Python REPL sessions."
  (mapcan (lambda (bn)
            (if (string-match "\\*python:\\(.*\\)\\*" bn)
                (list (match-string 1 bn))
              nil))
          (mapcar #'buffer-name (buffer-list))))

(defun py-vterm-interaction-repl (&optional arg)
  "Create an inferior Python REPL buffer and open it.
The buffer name will be `*python:main*' where `main' is the
default session name.  With prefix ARG, prompt for a session
name.  If there's already an alive REPL buffer for the session,
it will be opened."
  (interactive "P")
  (let* ((session-name
          (cond ((null arg) nil)
                (t (completing-read "Session name: " (py-vterm-interaction-repl-list-sessions) nil nil nil nil
                                    (py-vterm-interaction-repl-session-name (py-vterm-interaction-fellow-repl-buffer))))))
         (orig-buffer (current-buffer))
         (repl-buffer (py-vterm-interaction-repl-buffer session-name)))
    (if (and (boundp 'py-vterm-interaction-mode) py-vterm-interaction-mode)
        (with-current-buffer repl-buffer
          (setq py-vterm-interaction-repl-script-buffer orig-buffer)))
    (pop-to-buffer-same-window repl-buffer)))

(defun py-vterm-interaction-repl-switch-to-script-buffer ()
  "Switch to the script buffer that is paired with this Python REPL buffer."
  (interactive)
  (let ((repl-buffer (current-buffer))
        (script-buffer (if (buffer-live-p py-vterm-interaction-repl-script-buffer)
                           py-vterm-interaction-repl-script-buffer
                         nil)))
    (if script-buffer
        (with-current-buffer script-buffer
          (setq-local py-vterm-interaction-fellow-repl-buffer repl-buffer)
          (switch-to-buffer-other-window script-buffer)))))

(defun py-vterm-interaction-repl-restart ()
  "Restart the inferior Python process in the current REPL buffer."
  (interactive)
  (if (y-or-n-p "Restart Python REPL? ")
      (py-vterm-interaction-repl-buffer (py-vterm-interaction-repl-session-name (current-buffer)) t)))

(defun py-vterm-interaction-repl-clear-buffer ()
  "Clear the content of the Python REPL buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (vterm-clear 1)))

(defvar-local py-vterm-interaction-repl-filter-functions '()
  "List of filter functions that process the output to the REPL buffer.")

(defun py-vterm-interaction-repl-run-filter-functions-func (session)
  "Return a function that run registered filter functions for SESSION with args."
  (lambda (args)
    (with-current-buffer (py-vterm-interaction-repl-buffer session)
      (let ((proc (car args))
            (str (cadr args)))
        (let ((funcs py-vterm-interaction-repl-filter-functions))
          (while funcs
            (setq str (apply (pop funcs) (list str))))
          (list proc str))))))

(defun py-vterm-interaction-repl-prompt-status ()
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


(defun py-vterm-interaction--get-script-file (name)
  "Return the full path of the script file with NAME."
  (expand-file-name (concat "./scripts/" name ".py")
                    (file-name-directory (symbol-file 'py-vterm-interaction-mode))))

(defun py-vterm-interaction--execute-script (name &rest args)
  "Load the script with file NAME and call the eponymous function with ARGS.
Returns either the result of the function or nil if execution
times out after `py-vterm-interaction-repl-script-timeout'.

The script file is expected to be in the `scripts' directory of
the package and must contain exactly one function with the same
name as the file without the extension.  The function must return
a JSON-serializable object.  The function is called with the
arguments ARGS and the result is returned as a parsed JSON object
in the plist format.  The functions from the utility script are
loaded into the repl as well.  All loaded functions and modules
will be cleaned up afterwards."
  (let ((utility-file (py-vterm-interaction--get-script-file name))
        (script-file (py-vterm-interaction--get-script-file "utility"))
        (tmpfile (make-temp-file "py-vterm-interaction--" nil ".json"))
        (py-vterm-interaction-paste-with-return nil)
        (py-vterm-interaction-paste-with-clear nil)
        (arglist (concat
                  (seq-reduce
                   (lambda (el rest)
                     (format "%s, \"%s" el rest))
                   args "")
                  (if (seq-empty-p args) "" "\"")))
        (up-to-now (buffer-string))
        (result nil))

    (py-vterm-interaction-clear-line)
    (py-vterm-interaction-paste-string (format "exec(open(\"%s\").read());" utility-file))
    (py-vterm-interaction-paste-string (format "exec(open(\"%s\").read());" script-file))
    (py-vterm-interaction-paste-string (format "%s(\"%s\"%s);" name tmpfile arglist))

    ;; clean up all utility stuff
    (py-vterm-interaction-paste-string "del dump_json;")
    (py-vterm-interaction-paste-string (format "del %s" name))
    (py-vterm-interaction-send-return-key)

    (with-timeout (py-vterm-interaction-repl-script-timeout
                   (progn (display-warning 'py-vterm-interaction "Python script did not finish in time.")
                          (delete-file tmpfile)
                          nil))
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
        (py-vterm-interaction-repl-clear-buffer)
        (insert up-to-now))
      result)))


(defun py-vterm-interaction--read-script (name)
  "Read the content of the script file with NAME.
The path of the script is expanded relative to the `scripts' directory."
  (let ((script-file (py-vterm-interaction--get-script-file name)))
    (with-temp-buffer
      (insert-file-contents script-file)
      (buffer-string))))

(defun py-vterm-interaction-repl-get-context (buf)
  "Obtain context information of the REPL buffer BUF.
This returns a list of the current working directory of teh
inferior Python process and the current active environment."
  (with-current-buffer buf
    (let ((context (py-vterm-interaction--execute-script "get_env")))
      (plist-put context :script-buffer py-vterm-interaction-repl-script-buffer))))

(defvar py-vterm-interaction-repl-copy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") #'py-vterm-interaction-repl-copy-mode)
    (define-key map [return] #'py-vterm-interaction-repl-copy-mode-done)
    (define-key map (kbd "RET") #'py-vterm-interaction-repl-copy-mode-done)
    (define-key map (kbd "C-c C-r") #'vterm-reset-cursor-point)
    map))

(define-minor-mode py-vterm-interaction-repl-copy-mode
  "Toggle copy mode."
  :group 'py-vterm-interaction-repl
  :lighter " VTermCopy"
  :keymap py-vterm-interaction-repl-copy-mode-map
  (if py-vterm-interaction-repl-copy-mode
      (progn
        (message "Start copy mode")
        (use-local-map nil)
        (vterm-send-stop))
    (vterm-reset-cursor-point)
    (use-local-map py-vterm-interaction-repl-mode-map)
    (vterm-send-start)
    (message "End copy mode")))

(defun py-vterm-interaction-repl-copy-mode-done ()
  "Save the active region to the kill ring and exit copy mode."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (user-error "No active region"))
  (py-vterm-interaction-repl-copy-mode -1))


;;----------------------------------------------------------------------
(defgroup py-vterm-interaction nil
  "A minor mode to interact with an inferior Python REPL."
  :group 'python)

(defcustom py-vterm-interaction-hook nil
  "Hook run after starting a Python script buffer with an inferior Python REPL."
  :type 'hook
  :group 'py-vterm-interaction)

(defun py-vterm-interaction-fellow-repl-buffer (&optional session-name)
  "Return the paired REPL buffer or the one specified with SESSION-NAME.
If SESSION-NAME is not specified, try to return the currently
associated repl buffer or create a new one with the currently
associated session-name."
  (let ((session (or session-name py-vterm-interaction-session)))
    (if session
        (py-vterm-interaction-repl-buffer session-name)
      (if (buffer-live-p py-vterm-interaction-fellow-repl-buffer)
          py-vterm-interaction-fellow-repl-buffer
        (if py-vterm-interaction-session
            (py-vterm-interaction-repl-buffer py-vterm-interaction-session)
          (py-vterm-interaction-repl-buffer))))))

(defun py-vterm-interaction-switch-to-repl-buffer (&optional arg)
  "Switch to the paired REPL buffer or to the one with a specified session name.
With prefix ARG, prompt for session name.  If a session name is
provided all future commands will use that session."
  (interactive "P")
  (let* ((session-name
          (cond ((null arg) (or py-vterm-interaction-session nil))
                (t (completing-read "Session name: " (py-vterm-interaction-repl-list-sessions) nil nil nil nil
                                    (py-vterm-interaction-repl-session-name (py-vterm-interaction-fellow-repl-buffer))))))
         (script-buffer (current-buffer))
         (repl-buffer (py-vterm-interaction-fellow-repl-buffer session-name)))
    (setq py-vterm-interaction-fellow-repl-buffer repl-buffer)
    (setq py-vterm-interaction-session session-name)
    (with-current-buffer repl-buffer
      (setq py-vterm-interaction-repl-script-buffer script-buffer)
      (switch-to-buffer-other-window repl-buffer))))

(defun py-vterm-interaction-send-return-key ()
  "Send a return key to the Python REPL."
  (with-current-buffer (py-vterm-interaction-fellow-repl-buffer)
    (vterm-send-return)))

(defun py-vterm-interaction-send-backspace ()
  "Send a backspace key to the Python REPL."
  (with-current-buffer (py-vterm-interaction-fellow-repl-buffer)
    (vterm-send-backspace)))

(defun py-vterm-interaction-send-current-line ()
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
          (py-vterm-interaction-paste-string line)
          (if (not py-vterm-interaction-paste-with-return)
              (py-vterm-interaction-send-return-key))
          (if (not char)
              (newline))))))
  (forward-line))

(defun py-vterm-interaction-paste-string (string)
  "Send STRING to the Python REPL buffer using brackted paste mode."
  (with-current-buffer (py-vterm-interaction-fellow-repl-buffer)
    (goto-char (point-max))
    (when py-vterm-interaction-paste-with-clear
      (py-vterm-interaction-clear-line))
    (vterm-send-string string t)
    (if py-vterm-interaction-paste-with-return
        (py-vterm-interaction-send-return-key))))

(defun py-vterm-interaction-clear-line ()
  "Clear the current line in the Python REPL buffer."
  (with-current-buffer (py-vterm-interaction-fellow-repl-buffer)
    (goto-char (point-max))
    (vterm-send-key (kbd "C-a"))
    (vterm-send-key (kbd "C-k"))))

(defun py-vterm-interaction-ensure-newline (str)
  "Add a newline at the end of STR if the last character is not a newline."
  (concat str (if (string= (substring str -1 nil) "\n") "" "\n")))

(defun py-vterm-interaction-send-region-or-current-line ()
  "Send the content of the region if the region is active.
Otherwise, send the current line."
  (interactive)
  (if (use-region-p)
      (let ((str (buffer-substring-no-properties (region-beginning) (region-end))))
        (py-vterm-interaction--send-maybe-silent str "region")
        (deactivate-mark))
    (py-vterm-interaction-send-current-line)))

(defun py-vterm-interaction--load-file (file &optional comment)
  "Load the content of the file with FILE into the Python REPL buffer.
Optional argument COMMENT will be appended as a comment in the repl."
  (if (eq py-vterm-interaction-repl-interpreter :ipython)
      (format "%%run -i %s # %s" file (or comment ""))
    (format "exec(open(\"%s\").read()) # %s" file comment)))

(defun py-vterm-interaction--send-maybe-silent (string &optional comment)
  "Send STRING to the Python REPL buffer.
If PY-VTERM-INTERACTION--SEND-MAYBE-SILENT is non-nil, uses
`%run -i' with a temp file.  Optional argument COMMENT will be
appended as a comment in the repl."

  (with-current-buffer (py-vterm-interaction-fellow-repl-buffer)
    (if py-vterm-interaction-silent-cells
        (let ((tmpfile (make-temp-file "py-vterm-interaction" nil ".py")))
          (with-temp-file tmpfile
            (insert string))
          (py-vterm-interaction-paste-string (py-vterm-interaction--load-file tmpfile comment))
          (run-with-timer 10 nil
                          (lambda (tmpfile)
                            (delete-file tmpfile))
                          tmpfile))
      (py-vterm-interaction-paste-string string))))

(defun py-vterm-interaction-send-current-cell ()
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
        (py-vterm-interaction--send-maybe-silent cell-content cell-name)
        (when (not py-vterm-interaction-paste-with-return)
          (py-vterm-interaction-send-return-key))))))

(defun py-vterm-interaction-run-current-function ()
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
                  (py-vterm-interaction-paste-with-return nil)
                  (begin (region-beginning))
                  (end (region-end)))

              ;; capture decorators too
              (save-excursion
                (while
                    (progn (forward-line -1)
                           (beginning-of-line)
                           (if (looking-at "@")
                               (setq begin (point))
                             nil))))

              (let ((func (buffer-substring-no-properties begin end)))
                (py-vterm-interaction--send-maybe-silent func name)
                (py-vterm-interaction-send-return-key))

              (if (string-empty-p args)
                  (progn
                    (py-vterm-interaction-paste-string (format "%s()" name))
                    (py-vterm-interaction-send-return-key))
                (py-vterm-interaction-paste-string (format "%s(" name))))
          (message "No function found"))))))

(defun py-vterm-interaction-send-buffer ()
  "Send the whole content of the script buffer to the Python REPL line by line."
  (interactive)
  (let ((py-vterm-interaction-paste-with-return t)
        (py-vterm-interaction-paste-with-clear nil))

    (py-vterm-interaction-paste-string (buffer-string))))

(defun py-vterm-interaction-send-run-buffer-file ()
  "Run the current buffer file in the python vterm buffer.

This is equivalent to running `%run -i <buffer-file-name>` in the
python vterm buffer."
  (interactive)
  (let ((file buffer-file-name)
        (py-vterm-interaction-paste-with-return t))
    (with-current-buffer (py-vterm-interaction-fellow-repl-buffer)
      (py-vterm-interaction-paste-string (py-vterm-interaction--load-file file "load script buffer")))))

(defun py-vterm-interaction-send-import-buffer-file ()
  "Import the current buffer file like `from <module> import *' in the python repl."
  (interactive)
  (let ((file buffer-file-name)
        (py-vterm-interaction-paste-with-return t))
    (with-current-buffer (py-vterm-interaction-fellow-repl-buffer)
      (py-vterm-interaction--execute-script "star_import_script" file))))

(defun py-vterm-interaction-send-cd-to-buffer-directory ()
  "Change the REPL's working directory to the directory of the buffer file."
  (interactive)
  (if buffer-file-name
      (let ((buffer-directory (file-name-directory buffer-file-name)))
        (with-current-buffer (py-vterm-interaction-fellow-repl-buffer)
          (py-vterm-interaction-paste-string (if (eq py-vterm-interaction-repl-interpreter :ipython)
                                                 (format "%%cd %s" buffer-directory)
                                               (format "import os; os.chdir(\"%s\")" buffer-directory)))
          (setq default-directory buffer-directory)))
    (message "The buffer is not associated with a directory.")))

(defalias 'py-vterm-interaction-sync-wd #'py-vterm-interaction-send-cd-to-buffer-directory)

(defun py-vterm-interaction-fellow-repl-prompt-status ()
  "Return REPL mode or nil if REPL is not ready for input."
  (with-current-buffer (py-vterm-interaction-fellow-repl-buffer)
    (py-vterm-interaction-repl-prompt-status)))

;;;###autoload
(define-minor-mode py-vterm-interaction-mode
  "A minor for python script buffers to interact with an inferior Python REPL."
  :init-value nil
  :lighter " PY"
  :keymap
  `((,(kbd "C-c C-z") . ,#'py-vterm-interaction-switch-to-repl-buffer)
    (,(kbd "C-c C-c") . ,#'py-vterm-interaction-send-region-or-current-line)
    (,(kbd "C-c C-j") . ,#'py-vterm-interaction-send-current-cell)
    (,(kbd "C-c C-f") . ,#'py-vterm-interaction-run-current-function)
    (,(kbd "C-c C-i") . ,#'py-vterm-interaction-send-import-buffer-file)
    (,(kbd "C-c C-b") . ,#'py-vterm-interaction-send-buffer)
    (,(kbd "C-c C-r") . ,#'py-vterm-interaction-send-run-buffer-file)
    (,(kbd "C-c C-d") . ,#'py-vterm-interaction-send-cd-to-buffer-directory)))


(provide 'py-vterm-interaction)

;;; py-vterm-interaction.el ends here
