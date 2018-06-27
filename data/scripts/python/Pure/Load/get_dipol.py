import typing as typ
from dbgenPython.core.parsing import parse_line

def get_dipol(dftcode : str
             ,files   : typ.Dict[str,str]
             ) -> typ.Optional[str]:
    """
    Docstring
    """
    if dftcode =='vasp':

        incar = files['incar']
        for l in incar.split('\n'):
            if 'DIPOL' in l and 'LDIPOL' not in l:
                line   = parse_line(incar,'\nDIPOL',0)
                raw    = line.split('=')[-1].split()
                output = [float(x) for x in raw]
                return json.dumps(output)
        return None
    else:
        return None
