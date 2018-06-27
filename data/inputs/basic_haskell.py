import os

from dbgenPython.support.utils import *

###########
# Constants
############
script_pth   = os.environ['DBGEN_ROOT']+'/data/scripts/'
##############################################################################################
##############################################################################################
##############################################################################################
functions = {
	'get_dft_logfiles' 	: script_pth+'python/IO/get_dft_logfiles.py'
	,'testFun' 			: script_pth+'haskell/Test/TestFun.hs'
	,'testFun2' 			: script_pth+'haskell/Test/TestFun2.hs'
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

	,Transform(name = 'testFun'
				,ttype   = 'Insert'
				,table   = 'job'
				,cols    = ['timestamp','user']
				,outtype = ['Int','Str']
				,query   = 'select job.id,job.id from job'
				,intype  = ['Int']
				,template = Temp(tempfuncs = [TempFunc('testFun'
													  ,args=[STDIN()])
											 ,TempFunc('testFun2'
											 		  ,args=[TFA('testFun')])]
								,stdout    = [TFA('testFun2',index=0)
											 ,TFA('testFun2',index=1)]))

		]
