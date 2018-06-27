import typing as typ
def get_facet(params:typ.Dict[str,typ.Any])->str:
    """
    Extracts facet field from params.json
    """
    ############################################################################
    return params.get('facet','')
