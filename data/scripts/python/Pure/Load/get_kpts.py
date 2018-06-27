import typing as typ
import ast

from dbgenPython.core.parsing import parse_line

def get_kpts(dftcode : str
            ,files   : typ.Dict[str,str]
            ) -> typ.Tuple[int,int,int]:
    """
    docstring
    """
    if dftcode == 'gpaw':
        log    = files['log']
        parsed = parse_line(log,'kpts')
        raw    = parsed.split(': ')[-1].split()
        pure   = ','.join(raw)
        return tuple(ast.literal_eval(pure)) #type: ignore

    elif dftcode == 'quantumespresso':
        pwinp = files['pwinp']
        line  = pwinp.split('\n')[-2]
        raw   = line.split()[:3]
        return tuple([int(x) for x in raw])    #type: ignore

    elif dftcode == 'vasp':
        kptcar = files['kptcar']
        line = kptcar.split('\n')[-2]
        raw  = [int(x) for x in line.split()]
        return tuple(raw) #type: ignore
    else:
        raise NotImplementedError('new dftcode? ',dftcode)
