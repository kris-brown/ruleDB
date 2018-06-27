import typing as typ

def replacer(s            : str
            ,replace_dict : typ.Dict[str,str]
            ) -> str:
    """
    Executes a series of string replacement operations, specified by a
    dictionary
    """

    for k,v in replace_dict.items(): s = s.replace(k,v)

    return s
