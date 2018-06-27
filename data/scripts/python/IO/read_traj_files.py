import typing as typ
import ase              # type: ignore
from ase.io import read # type: ignore
from os.path import join

def read_traj_files(id           : int
                   ,stordir      : str
                   ,traj_to_json : typ.Callable[[ase.Atoms],str]
                   ) -> typ.List[typ.Tuple[str,int,int,int]]:
    """
    Helpful docstring
    """
    for f in ['init.traj','POSCAR']:
        try:
            init  = traj_to_json(read(join(stordir,f)))
            break
        except FileNotFoundError: pass

    for f in ['out.traj','final.traj','qn.traj','OUTCAR']:
        try:
            fin  = traj_to_json(read(join(stordir,f)))
            break
        except FileNotFoundError: pass

    return [(init,id,0,1),(fin,id,1,0)]
