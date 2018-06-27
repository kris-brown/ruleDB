import typing as typ

import json,glob

from os.path import join

def get_files(storpth : str) -> typ.Dict[str,str]:
    """
    This function condenses most of the I/O into one step.
        Grabs content of log file + pw.inp (if it exists)
        Grabs the name of anything with a traj or json extension
        Identifies whether or not there are vib.pckl files
    """


    def get(x:str)->str:
        try:
            with open(join(storpth,x),'r') as f:
                return f.read()
        except Exception as e:
            return ''

    def getGlob(x:str)->list:
        return glob.glob(join(storpth,x))


    vibpckls = json.dumps(getGlob('*.vib*.pckl'))
    return {'log'       : get('log')
           ,'qnlog'     : get('qn.log')
           ,'pwinp'     : get('pw.inp')
           ,'incar'     : get('INCAR')
           ,'poscar'    : get('POSCAR')
           ,'potcar'    : get('POTCAR')
           ,'kptcar'    : get('KPOINTS')
           ,'outcar'    : get('OUTCAR')
           ,'params'    : get('params.json')
           ,'vibpckls'  : vibpckls
           }
