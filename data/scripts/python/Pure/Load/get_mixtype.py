import typing as typ
from dbgenPython.core.parsing import parse_line

def get_mixtype(dftcode : str
               ,files   : typ.Dict[str,str]
               ) -> typ.Optional[str]:
    """
    Helpful docstring
    """

    if dftcode == 'quantumespresso':
        log    = files['log']
        parsed = parse_line(log,'number of iterations used',0)
        return parsed.split()[6]

    else:
        return None
