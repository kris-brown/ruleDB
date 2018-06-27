import typing as typ

def get_gamma(dftcode : str
             ,files   : typ.Dict[str,str]
             ) -> typ.Optional[int]:
    """
    Docstring
    """
    
    if dftcode =='vasp':
        kptcar = files['kptcar']
        return int('gamma' in kptcar)
    else :
        return None
