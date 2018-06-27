import typing as typ
from dbgenPython.core.parsing import parse_line

def get_econv(dftcode : str
             ,files   : typ.Dict[str,str]
             ) -> typ.Optional[float]:
    """
    Docstring
    """

    if dftcode == 'gpaw':
        log    = files['log']
        parsed = parse_line(log,'convergence:',0)
        raw    = parsed.split('energy:')[-1]
        pure   = raw.replace('}','')
        return float(pure)

    elif dftcode == 'quantumespresso':
        pwinp = files['pwinp']

        try:
            line = parse_line(pwinp,'conv_thr',-1)
        except IndexError:
            raise ValueError('Problem with parse line')

        raw = line.split('=')[1][:-1]
        return round(13.60569 * float(raw),6)

    elif dftcode == 'vasp':
        incar  = files['incar']
        parsed = parse_line(incar,'EDIFF',0)
        raw    = parsed.split('=')[-1]
        return float(raw)

    else:
        raise NotImplementedError("Unknown dft code?")
