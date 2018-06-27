import typing as typ
import json

def extract_cell(json_atoms : str)->typ.Tuple[float,float,float,float,float,float,float,float,float]:
    """
    Extracts information about a list of atoms (in json form) and prepares cell
    info for insertion into cell table
    """
    atoms = json.loads(json_atoms)
    [a,b,c] = atoms['cell']
    return  tuple(a+b+c) #type: ignore
