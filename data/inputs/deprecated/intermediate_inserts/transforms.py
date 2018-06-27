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
	'get_vib_modes' 	 : pypth+'Pure/Load/get_vib_modes.py'
	,'load_atoms' 		 : pypth+'Pure/Load/load_atoms.py'
	,'load_comp' 		 : pypth+'Pure/Load/load_comp.py'
	,'get_element_names' : pypth+'Pure/Load/get_element_names.py'
}

###############################################################################3

transforms = [Transform(name = 'populate_bulkmod_job'
			,ttype   = 'Insert'
			,table   = 'bulkmod_job'
			,cols    = ['job_id']
			,outtype = ['Int']
			,query   = "select job.id from job where job.job_type ='bulkmod'"
			,intype  = ['Int'])

    ,Transform(name = 'populate_neb_job'
    			,ttype   = 'Insert'
    			,table   = 'neb_job'
    			,cols    = ['job_id']
    			,outtype = ['Int']
    			,query   = "select job.id from job where job.job_type ='neb'"
    			,intype  = ['Int'])

    ,Transform(name = 'populate_vib_job'
    			,ttype   = 'Insert'
    			,table   = 'vib_job'
    			,cols    = ['job_id']
    			,outtype = ['Int']
    			,query   = "select job.id from job where job.job_type ='vib'"
    			,intype  = ['Int'])

    ,Transform(name = 'identify_neb'
    			,ttype   = 'Update'
    			,table   = 'neb_job'
    			,cols    = ['init','final']
    			,outtype = ['Int','Int']
    			,query   = "select relax_job.job_id,relax_job.job_id from relax_job"
    			,intype  = ['Int']
				,template = SimpleTemp('undefined',n_inputs=1))

    ,Transform(name = 'populate_vibmode'
    			,ttype   = 'Insert'
    			,table   = 'vibmode'
    			,cols    = ['vib_job_id','vib_ind','magnitude','real']
    			,outtype = ['Int','Int','Float','Int']
    			,query   = 'select vib_job.id,job.stordir from job join vib_job on vib_job.job_id = job.id'
    			,intype  = ['Int','Str']
    			,template = SimpleTemp('get_vib_modes',n_inputs=2))

    ,Transform(name = 'populate_atom'
    			,ttype   = 'Insert'
    			,table   = 'atom'
    			,cols    = ['structure_id','ind','element_id','x'
							,'y','z','constrained','magmom','tag']
    			,outtype = ['Int', 'Int', 'Int', 'Float'
						  ,'Float','Float','Int','Float','Int']
    			,query   = 'select structure.id,structure.raw_structure from structure'
    			,intype  = ['Int','Str']
    			,template = SimpleTemp('load_atoms',n_inputs=2))

    ,Transform(name = 'populate_composition'
    			,ttype   = 'Insert'
    			,table   = 'composition'
    			,cols    = ['structure_id','element_id',	'has'
						   ,'count',	   'count_ads', 	'count_fixed'
						   ,'frac', 	   'frac_ads', 		'frac_fixed'
						   ,'count_norm',  'count_ads_norm','count_fixed_norm']
    			,outtype = ['Int',   'Int',   'Int'
						   ,'Int',   'Int',   'Int'
						   ,'Float', 'Float', 'Float'
						   ,'Int',   'Int',   'Int']
    			,query   = ('select atom.structure_id,structure.raw_structure,relax_job.adsorbates '
							+'from atom join structure on structure.id = atom.structure_id '
							+'join relax_job on relax_job.job_id = structure.job_id')
    			,intype  = ['Int','Str','Str']
    			,template = SimpleTemp('load_comp',n_inputs=3))

	,Transform(name='populate_element'
		,ttype='Insert'
		,table = 'element'
		,cols=['symbol']
		,outtype=['Str']
		,template = SimpleTemp('get_element_names',n_inputs=0))

	]
