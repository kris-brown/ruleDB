import typing as typ
from dbgenPython.core.parsing import parse_line

def get_nmix(dftcode : str
            ,files   : typ.Dict[str,str]
            ) -> typ.Optional[float]:
    """
    Docstring
    """
    if dftcode == 'gpaw':
        log    = files['log']
        parsed = parse_line(log,'nmaxold',0)
        raw    = parsed.split(':')[1].replace(',','')
        return float(raw)

    elif dftcode == 'quantumespresso':
        pwinp  = files['pwinp']
        parsed = parse_line(pwinp,'mixing_ndim',0)
        raw    = parsed.split("=")[1][:-1]
        return round(float(raw),3)

    else:
        return None
