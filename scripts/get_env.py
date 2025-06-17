def get_env(tempfile):
    import os

    pyvterm_dump_json(
        tempfile,
        dict(
            env=["=".join(el) for el in list(os.environ.items())],
            cwd=os.getcwd(),
        ),
    )
