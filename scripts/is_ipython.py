def is_ipython(tempfile):
    try:
        return pyvterm_dump_json(tempfile, __IPYTHON__)
    except NameError:
        return pyvterm_dump_json(tempfile, False)
