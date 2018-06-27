import typing as typ

def load_refstoich()->typ.List[typ.Tuple[int,int,float,int]]:
    """
    Summarizes the stoichiometry of reference states for each element
    """
    output = []
    for i in range(1,119):
        if   i==6: output.extend([[6,6,1.0,3],[6,8,-1.0,3],[6,1,2.0,3]]) # H2O
        elif i==8: output.extend([[8,8,1.0,2],[8,1,-2.0,2]])           # CO
        else:      output.append([i,i,1.0,1])                        # pure species
    return [tuple(o) for o in output] # type: ignore
