def dump_json(tempfile, value):
    import json

    with open(tempfile, "w") as f:
        json.dump(value, f)
