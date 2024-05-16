def get_env(tempfile):
    import os

    dump_json(
        tempfile,
        dict(
            env=["=".join(el) for el in list(os.environ.items())],
            cwd=os.getcwd(),
        ),
    )
