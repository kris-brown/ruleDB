import ase
from ase.io import read
from os.path import join,exists

def get_traj(root : str
            ,nam  : str
            ) -> ase.Atoms:
    """
    ASE IO read function
    """
    pth = join(root,nam+'.traj')
    if not exists(pth):
        if 'final.traj' in pth:
            pth=pth.replace('final.traj','OUTCAR')
    if not exists(pth):
        pth = pth.replace('OUTCAR','out.traj')
    return read(pth)
