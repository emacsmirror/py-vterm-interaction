def is_ipython(tempfile):
    try:
        return dump_json(tempfile, __IPYTHON__)
    except NameError:
        return dump_json(tempfile, False)
