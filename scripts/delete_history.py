def delete_history(tmpfile, num):
    try:
        if not __IPYTHON__:
            dump_json(tmpfile, False)
            return

        hismgr = get_ipython().history_manager
        hismgr.db.execute(
            "DELETE from history where rowid in (Select rowid FROM history WHERE session={0} order by line DESC LIMIT {1})".format(
                hismgr.get_last_session_id(), num
            )
        )
        del hismgr
        dump_json(tmpfile, True)

    except NameError:
        dump_json(tmpfile, True)
