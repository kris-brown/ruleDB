import typing as typ
from dbgenPython.core.parsing import parse_line

def parse_psp(files  : typ.Dict[str,str]
            ,dftcode : str
            ) -> str:
    """
    Returns simplified string representation of the pseudopotnetial used in a
    DFT calculation
    """

    if dftcode == 'vasp':
        return '{}' #ideally we parse the potcar to reconstruct setups dictionary
    elif dftcode == 'gpaw':
        log   = files.get('log','')
        pspth = parse_line(log,'setups',0).split(': ')[-1].strip()
    elif dftcode == 'quantumespresso':
        pwinp = files.get('pwinp','')
        pspth = parse_line(pwinp,'pseudo_dir',0).split("='")[1][:-2]
    else:
        raise NotImplementedError("new dftcode? ",dftcode)

    if   'gbrv'   in pspth: return 'gbrv15pbe'
    elif 'dacapo' in pspth: return 'dacapo'
    elif 'sg15'   in pspth: return 'sg15'
    elif 'paw'    in pspth:
        if 'gpaw-setups-0.6.6300' in log: return 'oldpaw'
        else:                             return 'paw'
    else:
        raise NotImplementedError('New psp? path = ',pspth)
