import typing as typ
import ase.data

def get_element_names()->typ.List[str]:
    """
    List of chemical symbols, from H to Pb
    """
    return ase.data.chemical_symbols[1:83]
