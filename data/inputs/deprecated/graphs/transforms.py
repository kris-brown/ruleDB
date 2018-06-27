import os
from dbgenPython.support.utils import *
###########
# Constants
############
pypth   = os.environ['DBGEN_ROOT']+'/data/scripts/python/'

######
# Data
######

functions = {
	'get_bonds' 			: pypth+"IO/get_bonds.py"
	,'get_chargemol_atoms' 			: pypth+"IO/get_chargemol_atoms.py"

}


transforms = [
    Transform(name = 'populate_chargemol_job'
    			,ttype   = 'Insert'
    			,table   = 'chargemol_job'
    			,cols    = ['job_id']
    			,outtype = ['Int']
    			,query   = "select job.id from job where job.job_type ='chargemol'"
    			,intype  = ['Int'])

    ,Transform(name='populate_bond'
    			,ttype   = 'Insert'
    			,table   = 'bond'
    			,cols    = ['chargemol_job_id','bond_ind','ind1','ind2','bondorder']
    			,outtype = ['Int',             'Int',     'Int', 'Int', 'Float']
    			,query   = ("select chargemol_job.id,job.stordir "
                            +"from chargemol_job join job on "
                            +"chargemol_job.job_id = job.id")
    			,intype  = ['Int','Str']
                ,template = SimpleTemp('get_bonds',n_inputs=2))

    ,Transform(name='populate_chargemol_atom'
    			,ttype   = 'Insert'
    			,table   = 'chargemol_atom'
    			,cols    = ['chargemol_job_id','ind','charge']
    			,outtype = ['Int',             'Int','Float']
    			,query   = ("select chargemol_job.id,job.stordir "
                            +"from chargemol_job join job on "
                            +"chargemol_job.job_id = job.id")
    			,intype  = ['Int','Str']
                ,template = SimpleTemp('get_chargemol_atoms',n_inputs=2))

]
