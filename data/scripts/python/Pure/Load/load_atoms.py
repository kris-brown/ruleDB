import typing as typ
import json

def load_atoms(id           : int
              ,json_atoms   : str
              ) -> typ.List[typ.Tuple[int,int,int,float,float,float,int,float,int]]:
    """
    Unpack a list of json'd atoms objects into a form for insertion into the atom table
    """

    atoms_dict = json.loads(json_atoms)
    atoms = atoms_dict['atomdata']
    keys =  ['index','number','x','y','z','constrained','magmom','tag']
    return [tuple([id]+[a[k] for k in keys]) for a in atoms] # type: ignore
