import typing as typ
import os
from os.path import join

def get_bonds(id      : int
             ,stordir : str
             ) -> typ.List[typ.Tuple[int,int,int,int,float]]:
    """
    Parses a Chargemol output
    """

    bonds = 'DDEC6_even_tempered_bond_orders.txt'

    with open(join(stordir,bonds),'r') as f:
        f.read()

    raise NotImplementedError

    return []
