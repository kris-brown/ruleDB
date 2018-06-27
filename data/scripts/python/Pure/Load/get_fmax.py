import typing as typ
import json
from dbgenPython.core.parsing import parse_line

def get_fmax(dftcode     : str
            ,paramsdict  : str
            ,files       : typ.Dict[str,str]
            ) -> typ.Optional[float]:
    """
    docstring
    """
    if dftcode == 'vasp':
        incar = files['incar']
        return -float(parse_line(incar,'EDIFFG',0).split('=')[-1])
    else:
        params = json.loads(paramsdict)
        return params.get('fmax')     # no conceivable way of getting this from log
