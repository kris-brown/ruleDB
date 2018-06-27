import typing as typ
from dbgenPython.core.parsing import parse_line

def get_diag(dftcode : str
            ,files   : typ.Dict[str,str]
            ) -> typ.Optional[float]:
    """
    Docstring
    """

    if dftcode == 'gpaw':
        log       = files['log']
        parsed    = parse_line(log,'eigensolver',0)
        eigenline = parsed.split(': ')[-1]

        if 'dav' in eigenline:
            return 'david'
        else:
            return eigenline

    elif dftcode == 'quantumespresso':
        pwinp = files['pwinp']
        parsed = parse_line(pwinp,'diagonalization',0)
        return parsed.split("='")[1][:-2]

    else:
        return None
