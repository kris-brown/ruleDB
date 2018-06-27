import typing as typ
from dbgenPython.core.parsing import parse_line

def parse_dftcode(files : typ.Dict[str,str]) -> str:
    """
    gets a dftcode
    """
    if len(files.get('incar')) > 0:
        return 'vasp'
    else:
        log = files.get('log')

        if 'espresso' in log:
            return 'quantumespresso'

        elif 'gpaw' in log:
            return 'gpaw'

        else:
            raise ValueError("Can't parse DFTcode from \n\n\n"+log)
