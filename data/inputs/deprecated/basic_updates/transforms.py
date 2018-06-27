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
	'get_spacegroup' 	: pypth+'Pure/Atoms/get_spacegroup.py'
	,'get_pointgroup' 	: pypth+'Pure/Atoms/get_pointgroup.py'
	,'get_system_type'  : pypth+'Pure/Atoms/get_system_type.py'
	,'get_fwid' 		: pypth+'IO/get_fwid.py'
	,'get_eng' 			: pypth+'IO/get_eng.py'
	,'get_adsorbates' 	: pypth+'Pure/Load/get_adsorbates.py'
}

transforms = [
	Transform(name='spacegroup'
		,ttype   = 'Update'
		,table   = 'structure'
		,cols    = ['spacegroup']
		,outtype = ['Int']
		,query   = "select structure.id,structure.raw_structure from structure"
		,intype  = ['Str']
		,template = Temp(tempfuncs = [TempFunc('get_spacegroup'
											  ,args=[TFA('json_to_traj')])
									 ,TempFunc('json_to_traj'
									 		  ,args=[TFA()])
									 ]
						,stdout = [TFA('get_spacegroup')]))

	,Transform(name='pointgroup'
		,ttype   = 'Update'
		,table   = 'structure'
		,cols    = ['pointgroup']
		,outtype = ['Str']
		,query   = "select structure.id,structure.raw_structure from structure"
		,intype  = ['Str']
		,template = Temp(tempfuncs = [TempFunc('get_pointgroup'
											  ,args=[TFA('json_to_traj')])
									 ,TempFunc('json_to_traj'
									 		  ,args=[TFA()])
									 ]
						,stdout = [TFA('get_pointgroup')]))

	,Transform(name='system_type'
		,ttype   = 'Update'
		,table   = 'structure'
		,cols    = ['system_type']
		,outtype = ['Str']
		,query   = "select structure.id,structure.raw_structure from structure"
		,intype  = ['Str']
		,template = Temp(tempfuncs = [TempFunc('get_system_type'
											 ,args=[TFA('json_to_traj')])
									 ,TempFunc('json_to_traj'
									 		  ,args=[TFA()])
									 ]
						,stdout = [TFA('get_system_type')]))

	,Transform(name='fwid'
		,ttype   = 'Update'
		,table   = 'job'
		,cols    = ['fwid']
		,outtype = ['Int']
		,query   = "select job.id,job.stordir from job"
		,intype  = ['Str']
		,template = SimpleTemp('get_fwid'))

	,Transform(name='raweng'
		,ttype   = 'Update'
		,table   = 'relax_job'
		,cols    = ['raweng']
		,outtype = ['Float']
		,query   = ("select relax_job.job_id,job.stordir,calc.dftcode "
					+"from relax_job "
					+"join calc on calc_id = calc.id "
					+'join job on job_id = job.id')
		,intype  = ['Str','Str']
		,template = Temp(tempfuncs = [TempFunc('get_eng'
											 ,args=[TFA(index=0)
											 	   ,TFA(index=1)
												   ])]
						,stdout = [TFA('get_eng')]))

	# ,Transform(name='adsorbates'
	# 	,ttype   = 'Update'
	# 	,table   = 'relax_job'
	# 	,cols    = ['adsorbates']
	# 	,outtype = ['Str']
	# 	,query   = ('select relax_job.job_id,job.paramdict '
	# 			   +'from relax_job join job on relax_job.job_id = job.id')
	# 	,intype  = ['Str']
	# 	,template = SimpleTemp('get_adsorbates')
	# 	)
]
