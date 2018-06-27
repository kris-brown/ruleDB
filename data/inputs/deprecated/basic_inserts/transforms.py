import os
from dbgenPython.support.utils import *
###########
# Constants
############
pypth   = os.environ['DBGEN_ROOT']+'/data/scripts/python/'

calc_cols = ["raw_calc_other","kx","ky","kz"
			,"fmax","econv","dw","sigma"
			,"nbands","mixing","nmix","xtol"
			,"strain","gga","luse_vdw","zab_vdw"
			,"nelmdl","gamma","dipol","algo"
			,"ibrion","prec","ionic_steps","lreal"
			,"lvhar","diag","spinpol","dipole"
			,"maxstep","delta","mixingtype","bonded_inds"
			,"energy_cut_off","step_size","spring"]

calc_types = ["Str",		"Int",		 "Int",			"Int" 			# raw_calc
			 ,"Float",		"Float",	 Maybe("Float"), Maybe("Float") 		# fmax
             ,Maybe("Int"),	Maybe("Float"),	Maybe("Int"),Maybe("Float") 		# nbands
			 ,Maybe("Float"),Maybe("Str"),Maybe("Int"),	Maybe("Float") # strain
             ,Maybe("Int"),	Maybe("Int"), Maybe("Str"),	Maybe("Str")	# nelmdl
			 ,Maybe("Int"),	Maybe("Str"), Maybe("Int"), Maybe("Str") 	# ibrion
			 ,Maybe("Int"),	Maybe("Str"), Maybe("Int"),	Maybe("Int") 			# lvhar
			 ,Maybe("Int"),	Maybe("Float"),Maybe("Str"),Maybe("Str") 	# maxstep
             ,Maybe("Float"),Maybe("Float"),Maybe("Float")] 			# energy_cut_off

calc_tfas = ([TFA('get_kpts',index=i) for i in range(3)]
            +[TFA('get',id='fmax'),TFA('get_econv'),TFA('get_dw'),TFA('get_sigma')
		     ,TFA('get_nbands'),TFA('get_mixing'),TFA('get_nmix'),TFA('get',id='xtol')
		 	 ,TFA('get',id='strain'),TFA('get_vasp_key',id='gga'),TFA('get_vasp_key',id='luse_vdw'),TFA('get_vasp_key',id='zab_vdw')
		 	 ,TFA('get_vasp_key',id='nelmdl'),TFA('get_gamma'),TFA('get_dipol'),TFA('get_vasp_key',id='algo')
		 	 ,TFA('get_vasp_key',id='ibrion'),TFA('get_vasp_key',id='prec'),TFA('get_vasp_key',id='ionic_steps'),TFA('get_vasp_key',id='lreal')
		 	 ,TFA('get_vasp_key',id='lvhar'),TFA('get_diag'),TFA('get_spinpol'),TFA('get_dipole')
		 	 ,TFA('get_maxstep'),TFA('get',id='delta'),TFA('get_mixtype'),TFA('get',id='bonded_inds')
		 	 ,TFA('get',id='energy_cut_off'),TFA('get',id='step_size'),TFA('get',id='spring')])
######
# Data
######
functions = {
	'get_dft_logfiles' 		: pypth+'IO/get_dft_logfiles.py'
	,'get_job_metadata' 	: pypth+'IO/get_job_metadata.py'
	,'get_files' 			: pypth+"IO/get_files.py"
	,'get_traj' 			: pypth+"IO/get_traj.py"

	,"traj_to_json" 		: pypth+"Pure/Atoms/traj_to_json.py"
	,"json_to_traj" 		: pypth+"Pure/Atoms/json_to_traj.py"

	,'get_init_final_structure' : pypth+'Pure/Load/get_init_final_structure.py'
	,'extract_cell' 			: pypth+'Pure/Load/extract_cell.py'

	,'undefined' 		 	: pypth+'Pure/Misc/undefined.py'

	,'parse_dftcode' 		: pypth+"Pure/Load/parse_dftcode.py"
	,'parse_xc' 			: pypth+"Pure/Load/parse_xc.py"
	,'parse_pw' 			: pypth+"Pure/Load/parse_pw.py"
	,'parse_psp' 			: pypth+"Pure/Load/parse_psp.py"
	,'get_diag' 			: pypth+"Pure/Load/get_diag.py"
	,'get_dipol' 			: pypth+"Pure/Load/get_dipol.py"
	,'get_dipole' 			: pypth+"Pure/Load/get_dipole.py"
	,'get_dw' 				: pypth+"Pure/Load/get_dw.py"
	,'get_econv' 			: pypth+"Pure/Load/get_econv.py"
	,'get_fmax' 			: pypth+"Pure/Load/get_fmax.py"
	,'get_gamma' 			: pypth+"Pure/Load/get_gamma.py"
	,'get_kpts' 			: pypth+"Pure/Load/get_kpts.py"
	,'get_maxstep' 			: pypth+"Pure/Load/get_maxstep.py"
	,'get_mixing' 			: pypth+"Pure/Load/get_mixing.py"
	,'get_mixtype' 			: pypth+"Pure/Load/get_mixtype.py"
	,'get_nbands' 			: pypth+"Pure/Load/get_nbands.py"
	,'get_nmix' 			: pypth+"Pure/Load/get_nmix.py"
	,'get_sigma' 			: pypth+"Pure/Load/get_sigma.py"
	,'get_spinpol' 			: pypth+"Pure/Load/get_spinpol.py"
	,'get_vasp_key' 		: pypth+"Pure/Load/get_vasp_key.py"
	,'get' 					: pypth+"Pure/Load/get.py"
	,'raw_calc_other' 		: pypth+"Pure/Load/raw_calc_other.py"
}


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
									  	      ,args   = [TFA()]) # STORDIR
									 ]
					   ,stdout = [TFA(source = 'parse_dftcode')
					   			 ,TFA(source = 'parse_xc')
					   			 ,TFA(source = 'parse_pw')
					   			 ,TFA(source = 'parse_psp')
								 ])
		,fk_tab = "relax_job"
		,fk_col = "calc_id")


	,Transform(name='populate_relax_calc_other'
		,ttype    = 'Upsert'
		,table    = 'calc_other'
		,cols     = calc_cols
		,outtype  = calc_types
		,query    = ('select relax_job.job_id,job.stordir,job.paramdict,calc.dftcode '
		 			+'from relax_job join job on relax_job.job_id = job.id '
					+' join calc on calc.id = relax_job.calc_id')
		,intype   = ['Str','Str','Str']
		,template = Temp(tempfuncs = [TempFunc(name = 'get_files'
									  	      ,args   = [TFA(index=0)])

									,TempFunc(name= 'get_vasp_key' # 'get_prec'
											 ,id    = 'prec'
											 ,args  = [TFA(index=2) # dftcode
											 		  ,TFA('get_files')
													  ,TFC("'PREC'")
													  ,TFC("'str'")
													  ])

									,TempFunc(name= 'get_vasp_key' # 'get_luse_vdw'
											 ,id    = 'luse_vdw'
											 ,args  = [TFA(index=2) # dftcode
											 		  ,TFA('get_files')
													  ,TFC("'LUSE_VDW'")
													  ,TFC("'bool'")
													  ])

									,TempFunc(name= 'get_vasp_key' # 'get_luse_vdw'
											 ,id    = 'gga'
											 ,args  = [TFA(index=2) # dftcode
											 		  ,TFA('get_files')
													  ,TFC("'GGA'")
													  ,TFC("'bool'")
													  ])

									,TempFunc(name= 'get_vasp_key' # 'get_lvhar'
											 ,id    = 'lvhar'
											 ,args  = [TFA(index=2) # dftcode
											 		  ,TFA('get_files')
													  ,TFC("'LVHAR'")
													  ,TFC("'bool'")
													  ])

									,TempFunc(name= 'get_vasp_key' # 'get_zab_vdw'
											 ,id    = 'zab_vdw'
											 ,args  = [TFA(index=2) # dftcode
											 		  ,TFA('get_files')
													  ,TFC("'ZAB_VDW'")
													  ,TFC("'float'")
													  ])

									,TempFunc(name= 'get_vasp_key' # 'get_algo'
											 ,id    = 'algo'
											 ,args  = [TFA(index=2) # dftcode
											 		  ,TFA('get_files')
													  ,TFC("'ALGO'")
													  ,TFC("'str'")
													  ])

									,TempFunc(name= 'get_vasp_key' # 'get_nelmdl'
											 ,id    = 'nelmdl'
											 ,args  = [TFA(index=2) # dftcode
											 		  ,TFA('get_files')
													  ,TFC("'NELMDL'")
													  ,TFC("'int'")
													  ])

									,TempFunc(name= 'get_vasp_key' # 'get_ibrion'
											 ,id    = 'ibrion'
											 ,args  = [TFA(index=2) # dftcode
											 		  ,TFA('get_files')
													  ,TFC("'IBRION'")
													  ,TFC("'int'")
													  ])

									,TempFunc(name= 'get_vasp_key' # 'get_lreal'
											 ,id    = 'lreal'
											 ,args  = [TFA(index=2) # dftcode
											 		  ,TFA('get_files')
													  ,TFC("'LREAL'")
													  ,TFC("'str'")
													  ])

									,TempFunc(name= 'get_vasp_key' # 'get_ionic_steps'
											 ,id    = 'ionic_steps'
											 ,args  = [TFA(index=2) # dftcode
											 		  ,TFA('get_files')
													  ,TFC("'NSW'")
													  ,TFC("'int'")
													  ])

									,TempFunc(name='get_dipol'
											 ,args  = [TFA(index=2)
											 		  ,TFA('get_files')
													  ])

									,TempFunc(name='get_gamma'
											 ,args  = [TFA(index=2)
											 		  ,TFA('get_files')])

									,TempFunc(name='get_econv'
											 ,args  = [TFA(index=2)
											 		  ,TFA('get_files')
													  ])

									,TempFunc(name='get_dw'
											 ,args  = [TFA(index=2)
											 		  ,TFA('get_files')
													  ])

									,TempFunc(name='get_sigma'
											 ,args  = [TFA(index=2)
											 		  ,TFA('get_files')
													  ])

									,TempFunc(name='get_mixing'
											 ,args  = [TFA(index=2)
											 		  ,TFA('get_files')
													  ])

									,TempFunc(name='get_nbands'
											 ,args  = [TFA(index=2)
											 		  ,TFA(index=1)
											 		  ,TFA('get_files')
													  ])

									,TempFunc(name='get_mixtype'
											 ,args  = [TFA(index=2)
											 		  ,TFA('get_files')
													  ])

									,TempFunc(name='get_nmix'
											 ,args  = [TFA(index=2)
											 		  ,TFA('get_files')
													  ])

									,TempFunc(name='get_diag'
											 ,args  = [TFA(index=2)
											 		  ,TFA('get_files')
													  ])

									,TempFunc(name='get_spinpol'
											 ,args  = [TFA(index=2)
											 		  ,TFA('get_files')
													  ])

									,TempFunc(name='get_dipole'
											 ,args  = [TFA(index=2)
											 		  ,TFA('get_files')
													  ])

									,TempFunc(name='get_maxstep'
											 ,args  = [TFA(index=2)
											 		  ,TFA('get_files')
													  ])

									,TempFunc(name="get"
											 ,id    = 'fmax'
											 ,args  = [TFA(index=1)
											 		  ,TFC("'fmax'")
													  ,TFC("'float'")
													  ])

									,TempFunc(name="get"
											 ,id    = 'xtol'
											 ,args  = [TFA(index=1)
											 		  ,TFC("'xtol'")
													  ,TFC("'float'")
													  ])

									,TempFunc(name="get"
											 ,id    = 'strain'
											 ,args  = [TFA(index=1)
											 		  ,TFC("'strain'")
													  ,TFC("'float'")
													  ])

									,TempFunc(name="get"
											 ,id    = 'energy_cut_off'
											 ,args  = [TFA(index=1)
											 		  ,TFC("'energy_cut_off'")
													  ,TFC("'float'")
													  ])

									,TempFunc(name="get"
											 ,id    = 'bonded_inds'
											 ,args  = [TFA(index=1)
											 		  ,TFC("'bonded_inds'")
													  ,TFC("'str'")
													  ])

									,TempFunc(name="get"
											 ,id    = 'step_size'
											 ,args  = [TFA(index=1)
											 		  ,TFC("'step_size'")
													  ,TFC("'float'")
													  ])

									,TempFunc(name="get"
											 ,id    = 'spring'
											 ,args  = [TFA(index=1)
											 		  ,TFC("'spring'")
													  ,TFC("'float'")
													  ])

									,TempFunc(name="get"
											 ,id    = 'delta'
											 ,args  = [TFA(index=1)
											 		  ,TFC("'delta'")
													  ,TFC("'float'")
													  ])

									,TempFunc(name="get_kpts"
											 ,args  = [TFA(index=2)
											 		  ,TFA('get_files')
													  ])

									,TempFunc(name="raw_calc_other"
											 ,args  = calc_tfas)
									]

					   ,stdout = [TFA('raw_calc_other')]+calc_tfas)
		,fk_tab = "relax_job"
		,fk_col = "calc_other_id")



	# ,Transform(name = 'populate_init_final_structure'
	# 	,ttype   = 'Insert'
	# 	,table   = 'structure'
	# 	,cols    = ['raw_structure']
	# 	,outtype = ['Str']
	# 	,query   = ('select relax_job.job_id,job.stordir '
	# 				+'from relax_job inner join job '
	# 					+'on relax_job.job_id=job.id')
	# 	,intype   = ['Int',"Str"]
	# 	,template = Temp(tempfuncs = [TempFunc(name = 'get_init_final_structure'
	# 										  ,args   = [TFA(index=0) # id
	# 										  		    ,TFA('get_traj',id='init') # ase.Atoms
	# 												    ,TFA('get_traj',id='final') # ase.Atoms
	# 												    ,FuncArg('traj_to_json')])
	# 								,TempFunc(name = 'get_traj'
	# 										 ,id     = 'init'
	# 										 ,args   = [TFA(index=1) # path
	# 												   ,TFC("'init'")])
	# 								,TempFunc(name = 'get_traj'
	# 										 ,id     = 'final'
	# 										 ,args   = [TFA(index=1) # path
	# 												   ,TFC("'final'")])
	# 								 ]
	# 				   ,stdout = [TFA(source = 'get_init_final_structure')]))

	,Transform(name = 'populate_cell'
		,ttype 	  = 'Upsert'
		,table 	  = 'cell'
		,cols 	  = ['ax','ay','az','bx','by','bz','cx','cy','cz']
		,outtype  = ['Float']*9
		,query    = 'select structure.id,structure.raw_structure from structure'
		,intype   = ['Str']
		,template = SimpleTemp('extract_cell',n_outputs=9)
		,fk_tab   = 'structure'
		,fk_col   = 'cell_id')

		]
