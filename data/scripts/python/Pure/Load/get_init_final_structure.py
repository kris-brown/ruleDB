import typing as typ
import ase

def get_init_final_structure(i          : int
                            ,init       : ase.Atoms
                            ,fin        : ase.Atoms
                            ,traj2json  : typ.Callable[[ase.Atoms],str]
                            ) -> typ.List[typ.Tuple[int,str,int,int]]:
    """
    Output columns should be: "relaxjobId","raw_structure","is_init","is_final"
    """
    
    initjsn = traj2json(init)
    finjsn  = traj2json(fin)

    return [(i,initjsn,1,0)
           ,(i,finjsn, 0,1)]
