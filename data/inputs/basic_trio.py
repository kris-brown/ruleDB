import os

from dbgenPython.support.utils import Transform,Table,Col,FK,SimpleTemp,Temp,TempFunc,TFA,STDIN

###########
# Constants
############
pypth   = os.environ['DBGEN_ROOT']+'/data/scripts/python/'
##############################################################################################
##############################################################################################
##############################################################################################
functions = {
	'get_dft_logfiles' 		: pypth+'IO/get_dft_logfiles.py'
	,'get_job_metadata' 	: pypth+'IO/get_job_metadata.py'
	,'get_files' 			: pypth+"IO/get_files.py"
	,'parse_dftcode' 		: pypth+"Pure/Load/parse_dftcode.py"
	,'parse_xc' 			: pypth+"Pure/Load/parse_xc.py"
	,'parse_pw' 			: pypth+"Pure/Load/parse_pw.py"
	,'parse_psp' 			: pypth+"Pure/Load/parse_psp.py"
}

##############################################################################################
##############################################################################################
##############################################################################################

tables = [
	Table('job'
		,desc='DFT jobs'
		,cols = [Col('id',pk=True)
			    ,Col('stordir','str',nn=True,uniq=True)
			    ,Col('user','str')
			    ,Col('timestamp')
			    ,Col('storage_directory','str')
			    ,Col('working_directory','str')
			    ,Col('paramdict','str')
				,Col('job_type','str') # used to partition subclasses
				])

	,Table('relax_job'
		,desc = 'Jobs that compute local minima for electronic energy'
		,cols = [Col('job_id',pk=True)]
		,fks  = [FK('job_id','job')
			    ,FK('calc_id','calc')])

	,Table('calc'
		,desc='DFT calc parameters'
		,cols = [Col('id',pk=True)
				,Col('dftcode','str',nn=True,uniq=True)
				,Col('xc','str',nn=True,uniq=True)
				,Col('pw','str',nn=True,uniq=True)
				,Col('psp','str',nn=True,uniq=True)])
]

##############################################################################################
##############################################################################################
##############################################################################################

transforms = [Transform(name = 'populate_job'
			,ttype   = 'Insert'
			,table   = 'job'
			,cols    = ['stordir']
			,outtype = ['Str']
			,query   = None
			,intype  = []
			,template = SimpleTemp('get_dft_logfiles',n_inputs=0))

	,Transform(name='get_job_metadata'
		,ttype   = 'Update'
		,table   = 'job'
		,cols    = ['user','timestamp','working_directory'
				   ,'storage_directory','paramdict','job_type']
		,outtype = ['Str','Int','Str'
				   ,'Str','Str','Str']
		,query   = "select job.id,job.stordir from job"
		,intype  = ['Str']
		,template = SimpleTemp('get_job_metadata',n_outputs=6))

	,Transform(name = 'populate_relax_job'
		,ttype   = 'Insert'
		,table   = 'relax_job'
		,cols    = ['job_id']
		,outtype = ['Int']
		,query   = ("select job.id from job "
					+ "where job.job_type in ('relax','latticeopt','vc-relax')")
		,intype  = ["Int"])

	,Transform(name='populate_relax_calc'
		,ttype    = 'Upsert'
		,table    = 'calc'
		,cols     = ["dftcode","xc","pw","psp"]
		,outtype  = ["Str","Str","Int","Str"]
		,query    = ('select relax_job.job_id,job.stordir '
					+'from relax_job join job on relax_job.job_id = job.id')
		,intype   = ['Str']
		,template = Temp(tempfuncs = [TempFunc(name = 'parse_dftcode'
											  ,args  = [TFA('get_files')
													   ])

									 ,TempFunc(name = 'parse_xc'
							 				  ,args  = [TFA('get_files')
											   		   ,TFA('parse_dftcode')
							 						   ])

									 ,TempFunc(name = 'parse_pw'
							 				  ,args   = [TFA('get_files')
											   		    ,TFA('parse_dftcode')
							 						    ]) # how can I guarantee that this is the function I want?

									 ,TempFunc(name = 'parse_psp'
							 				  ,args   = [TFA('get_files')
											   		    ,TFA('parse_dftcode')
							 						    ])

								     ,TempFunc(name = 'get_files'
									  	      ,args   = [STDIN(0)])
									 ]
					   ,stdout = [TFA(source = 'parse_dftcode')
					   			 ,TFA(source = 'parse_xc')
					   			 ,TFA(source = 'parse_pw')
					   			 ,TFA(source = 'parse_psp')
								 ])
		,fk_tab = "relax_job"
		,fk_col = "calc_id")

		]
