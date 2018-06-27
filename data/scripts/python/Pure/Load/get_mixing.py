import typing as typ
from dbgenPython.core.parsing import parse_line

def get_mixing(dftcode : str
              ,files   : typ.Dict[str,str]
              ) -> typ.Optional[float]:
    """
    Helpful docstring
    """

    if dftcode == 'gpaw':
        log = files['log']
        parsed = parse_line(log,'Linear mixing parameter',0)
        raw    = parsed.split(':')[1]
        return float(raw)

    elif dftcode == 'quantumespresso':
        pwinp = files['pwinp']
        parsed = parse_line(pwinp,'mixing_beta',0)
        raw    = parsed.split("=")[1][:-3]
        return round(float(raw),3)

    else:
        return None
