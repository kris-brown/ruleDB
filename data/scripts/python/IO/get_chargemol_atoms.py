import typing as typ
import os
from os.path import join

def get_chargemol_atoms(id      : int
             ,stordir : str
             ) -> typ.List[typ.Tuple[int,int,int,int,float]]:
    """
    Parses a Chargemol output
    """

    chrgs = 'DDEC6_even_tempered_net_atomic_charges.txt'

    with open(join(stordir,chrgs),'r') as f:
        f.read()

    raise NotImplementedError

    return []
