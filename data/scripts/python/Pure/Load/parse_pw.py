import typing as typ
from dbgenPython.core.parsing import parse_line

def parse_pw(files   : typ.Dict[str,str]
            ,dftcode : str
            ) -> int:
    """
    Using "dftcode" as a switch, choose an appropriate file from a directory
    and parse it for the planewave cutoff (in eV).

    files   - a dictionary mapping file names to file content
    dftcode - must be one of {gpaw,vasp,quantumespresso}
    parse_line
            - args are: haystack,needle,n
            - returns: the Nth line of a haystack that contains the needle
    """
    # Constants
    #----------
    RYDBERG_TO_EV = 13.60569

    # Parsing
    #-------
    if dftcode == 'gpaw':
        log     = files.get('log')
        parsed  = parse_line(log,'ecut',0)
        raw_num = parsed.split()[-1].replace(',','')

    elif dftcode == 'quantumespresso':
        pwinp       = files.get('pwinp')
        parsed      = parse_line(pwinp,'ecutwfc',0)
        raw_rydberg = parsed.split('=')[1][:7]
        raw_num     = RYDBERG_TO_EV * float(raw_rydberg)

    elif dftcode == 'vasp':
        incar   = files.get('incar')
        parsed  = parse_line(incar,'ENCUT',0)
        raw_num = parsed.split('=')[-1]
    else:
        raise ValueError('new dftcode? ',dftcode)

    return int(round(float(raw_num)))
