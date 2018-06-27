import typing as typ
from dbgenPython.core.parsing import parse_line

def get_dipole(dftcode : str
              ,files   : typ.Dict[str,str]
              ) -> int:
    """
    Docstring
    """

    if dftcode == 'gpaw':
        return 0

    elif dftcode == 'quantumespresso':
        pwinp = files['pwinp']
        return int('dipfield=.true.' in pwinp)

    elif dftcode == 'vasp':
        incar  = files['incar']
        parsed = parse_line(incar,'LDIPOL',0)
        return int('TRUE' in parsed)

    else:
        raise NotImplementedError
