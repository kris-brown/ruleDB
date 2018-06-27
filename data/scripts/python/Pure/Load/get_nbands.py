import typing as typ
import json
from dbgenPython.core.parsing import parse_line

def get_nbands(dftcode     : str
              ,paramsdict  : str
              ,files       : typ.Dict[str,str]
              ) -> typ.Optional[float]:
    """
    docstring
    """
    if dftcode == 'vasp':
        outcar = files['outcar']
        return int(parse_line(outcar,'NBANDS',0).split('=')[-1])
    else:
        params = json.loads(paramsdict)
        return params.get('fmax')     # no conceivable way of getting this from log
