"""Import all public names from a module into the current namespace."""


def star_import_script(tmpfile, path):
    import importlib, pathlib, sys

    name = pathlib.Path(path).stem
    spec = importlib.util.spec_from_file_location(name, path)
    mdl = importlib.util.module_from_spec(spec)
    sys.modules[name] = mdl
    spec.loader.exec_module(mdl)

    if "__all__" in mdl.__dict__:
        names = mdl.__dict__["__all__"]
    else:
        names = [x for x in mdl.__dict__ if not x.startswith("_")]

    # now drag them in
    globals().update({k: getattr(mdl, k) for k in names})

    dump_json(tmpfile, True)
