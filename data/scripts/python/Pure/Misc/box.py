import typing as typ

def box(x: typ.Any)->typ.List[typ.Any]:
    """Boxes a value as a singleton list"""
    return [x]
