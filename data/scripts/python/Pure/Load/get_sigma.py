import typing as typ
from dbgenPython.core.parsing import parse_line

def get_sigma(dftcode : str
             ,files   : typ.Dict[str,str]
             ) -> typ.Optional[float]:
    """
    Helpful docstring
    """

    if dftcode == 'gpaw':
        log = files['log']
        parsed = parse_line(log,'Fermi-Dirac',0)
        raw = parsed.split('=')[1].split()[0]
        return float(raw)

    elif dftcode == 'quantumespresso':
        pwinp = files['pwinp']
        parsed = parse_line(pwinp,'degauss',0)
        raw = parsed.split('=')[1][:7]
        return round(13.60569 * float(raw),3)

    elif dftcode == 'vasp':
        incar = files['incar']
        parsed = parse_line(incar,'SIGMA',0)
        raw    = parsed.split('=')[-1]
        return float(raw)

    else:
        raise NotImplementedError
