import json

def get_adsorbate(ind     : int
                 ,ads_raw : str
                 ) -> int: # type: ignore
    """
    Helpful docstring
    """
    if ads_raw in [None,'{}','[]']: return -1

    for i,subset in enumerate(json.loads(ads_raw)):
        if ind in subset: return i
    return -1
