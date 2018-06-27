import typing as typ

def get_parent(params:typ.Dict[str,typ.Any])->str:
    """
    Extracts structure field from params.json
    """
    import json
    ############################################################################
    return params.get('parent','')
