import typing as typ
from dbgenPython.core.parsing import parse_line

def get_maxstep(dftcode : str
               ,files   : typ.Dict[str,str]
               ) -> int :
    """
    Helpful docstring
    """

    if dftcode == 'gpaw':
        log    = files['log']
        parsed = parse_line(log,'maxiter',0)
        return int(parsed.split(':')[-1])

    elif dftcode == 'quantumespresso':
        pwinp  = files['pwinp']
        parsed = parse_line(pwinp,'electron_maxstep',0)
        return int(parsed.split('=')[1][:-1])

    elif dftcode == 'vasp':
        incar  = files['incar']
        parsed = parse_line(incar,'NELM',0)
        return int(parsed.split('=')[-1])

    else:
        raise NotImplementedError
