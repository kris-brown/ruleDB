import typing as typ
from dbgenPython.core.parsing import parse_line

def get_spinpol(dftcode : str
               ,files   : typ.Dict[str,str]
               ) -> typ.Optional[float]:
    """
    Docstring
    """

    if dftcode == 'gpaw':
        log    = files['log']
        parsed = parse_line(log,'spinpol',0)
        return int('True' in parsed)

    elif dftcode == 'quantumespresso':
        pwinp = files['pwinp']
        return int('starting_magnetization' in pwinp)

    elif dftcode == 'vasp':
        incar  = files['incar']
        parsed = parse_line(incar,'ISPIN',0)
        return int('1' in parsed)

    else:
        raise NotImplementedError
