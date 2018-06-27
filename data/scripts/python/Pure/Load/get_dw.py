import typing as typ
from dbgenPython.core.parsing import parse_line

def get_dw(dftcode : str
          ,files   : typ.Dict[str,str]
          ) -> typ.Optional[int]:
    """
    Docstring
    """
    if  dftcode == 'quantumespresso':
        pwinp  = files['pwinp']
        parsed = parse_line(pwinp,'ecutrho',0)
        raw    = parsed.split('=')[1][:7]
        return round(13.60569 * float(raw))
    else:
        return None
