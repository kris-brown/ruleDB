import typing as typ
import json
def raw_calc_other(*args : typ.Any) -> str:
    """
    Converts a raw calc into a string.
    This is necessary so that we can impose a UNIQUE constraint in SQLite
    when some columns can be NULL
    """
    def encode(s : str)->tuple:
        return tuple(map(ord,str(s)))

    return json.dumps([encode(x) for x in args])
