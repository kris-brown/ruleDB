import typing as typ
import json

def jsonloads(x:str)->typ.Any:
    """
    Parses a JSON string
    """
    return json.loads(x)
