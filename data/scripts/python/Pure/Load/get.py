import typing as typ

import json
from dbgenPython.core.misc import cast_maybe

def get(dictstr: str
       ,key    : str
       ,dtype  : str
       ) -> typ.Any:

    """
    Gets a key from a json'd dictionary
    """

    assert dtype in cast_maybe.keys(), 'bad dtype argument for get_vasp_key: '+dtype

    d = json.loads(dictstr)

    out = d.get(key)

    return cast_maybe[dtype](out)
