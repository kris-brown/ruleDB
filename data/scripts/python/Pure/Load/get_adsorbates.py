import json

def get_adsorbates(params_json:str)->str:
    """
    Extracts adsorbates field from params.json
    """
    params = json.loads(params_json)
    return params.get('adsorbates','[]')
