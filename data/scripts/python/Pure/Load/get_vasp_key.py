import typing as typ
from dbgenPython.core.parsing import parse_line
from dbgenPython.core.misc    import cast_maybe

def get_vasp_key(dftcode : str
                ,files   : typ.Dict[str,str]
                ,key     : str
                ,dtype   : str
                ) -> typ.Optional[typ.Union[int,str,float]]:
    """
    common form of extracting VASP input info
    """


    assert dtype in cast_maybe.keys(), 'bad dtype argument for get_vasp_key: '+dtype

    if dftcode =='vasp':

        incar  = files['incar']

        try:
            parsed = parse_line(incar,key,0).split('=')[-1].strip()
        except IndexError:
            parsed = None # type: ignore

        return cast_maybe[dtype](parsed) # type: ignore

    else:
        return None
