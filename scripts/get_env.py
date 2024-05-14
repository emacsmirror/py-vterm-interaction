def python_vterm__get_context(tempfile):
    import json
    import os

    with open(tempfile, "w") as f:
        context = dict(
            env=["=".join(el) for el in list(os.environ.items())],
            cwd=os.getcwd(),
        )
        json.dump(context, f)

    del os, json
