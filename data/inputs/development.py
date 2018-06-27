import os

from dbgenPython.support.utils import *

###########
# Constants
############
pypth   = os.environ['DBGEN_ROOT']+'/data/scripts/python/'

calc_cols = ["kx","ky","kz"
			,"fmax","econv","dw","sigma"
			,"nbands","mixing","nmix","xtol"
			,"strain","gga","luse_vdw","zab_vdw"
			,"nelmdl","gamma","dipol","algo"
			,"ibrion","prec","ionic_steps","lreal"
			,"lvhar","diag","spinpol","dipole"
			,"maxstep","delta","mixingtype","bonded_inds"
			,"energy_cut_off","step_size","spring"]

calc_types = ["Str","Int","Int","Int" 					# raw_calc
			 ,"Float","Float", "Float", "Float" 		# fmax
             ,"Int",	"Float",	"Int","Float" 		# nbands
			 ,"Float","Str","Int",	"Float" 			# strain
             ,"Int",	"Int", "Str",	"Str"			# nelmdl
			 ,"Int",	"Str", "Int", "Str" 			# ibrion
			 ,"Int",	"Str", "Int",	"Int" 			# lvhar
			 ,"Int",	"Float","Str","Str" 			# maxstep
             ,"Float","Float","Float"] 					# energy_cut_off

calc_tfas = ([TFA('get_kpts',index=i) for i in range(3)]
            +[TFA('get',id='fmax'),TFA('get_econv'),TFA('get_dw'),TFA('get_sigma')
		     ,TFA('get_nbands'),TFA('get_mixing'),TFA('get_nmix'),TFA('get',id='xtol')
		 	 ,TFA('get',id='strain'),TFA('get_vasp_key',id='gga'),TFA('get_vasp_key',id='luse_vdw'),TFA('get_vasp_key',id='zab_vdw')
		 	 ,TFA('get_vasp_key',id='nelmdl'),TFA('get_gamma'),TFA('get_dipol'),TFA('get_vasp_key',id='algo')
		 	 ,TFA('get_vasp_key',id='ibrion'),TFA('get_vasp_key',id='prec'),TFA('get_vasp_key',id='ionic_steps'),TFA('get_vasp_key',id='lreal')
		 	 ,TFA('get_vasp_key',id='lvhar'),TFA('get_diag'),TFA('get_spinpol'),TFA('get_dipole')
		 	 ,TFA('get_maxstep'),TFA('get',id='delta'),TFA('get_mixtype'),TFA('get',id='bonded_inds')
		 	 ,TFA('get',id='energy_cut_off'),TFA('get',id='step_size'),TFA('get',id='spring')])
##############################################################################################
##############################################################################################
##############################################################################################
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
			    ,Col('paramdict','text')
				,Col('job_type','str') # used to partition subclasses
				])

	,Table('relax_job'
		,desc = 'Jobs that compute local minima for electronic energy'
		,cols = [Col('job_id',pk=True)]
		,fks  = [FK('job_id','job')
			    ,FK('calc_id','calc')
			    ,FK('calc_other_id','calc_other')])

	,Table('vib_job'
		,desc = 'Jobs that compute vibrational modes for a structure'
		,cols = [Col('job_id',pk=True)]
		,fks  = [FK('job_id','job')
			    ,FK('structure_id','structure')
			    ,FK('calc_id','calc')
			    ,FK('calc_other_id','calc_other')])
	,Table('bulkmod_job'
		,desc = 'Jobs that compute bulk modulus for a structure'
		,cols = [Col('job_id',pk=True),Col('bulkmod','float')]
		,fks  = [FK('job_id','job')
			    ,FK('structure_id','structure')
			    ,FK('calc_id','calc')
			    ,FK('calc_other_id','calc_other')])

	,Table('chargemol_job'
		,desc = 'Jobs that compute bond order analysis with Chargemol'
		,cols = [Col('job_id',pk=True)]
		,fks = [FK('job_id','job')
			   ,FK('structure_id','structure')
			   ,FK('calc_id','calc')
			   ,FK('calc_other_id','calc_other')])

	,Table('calc'
		,desc='DFT calc parameters'
		,cols = [Col('id',pk=True)
				,Col('dftcode','str',nn=True,uniq=True)
				,Col('xc','str',nn=True,uniq=True)
				,Col('pw','str',nn=True,uniq=True)
				,Col('psp','str',nn=True,uniq=True)])

	,Table('calc_other'
		,desc = 'less important DFT Calculator Parameters'
		,cols = [Col('id',						pk=True)
				,Col("kx"						)
				,Col("ky"						)
				,Col("kz"						)
				,Col("fmax",			'float')
				,Col("econv",			'float')
				,Col("dw"						)
				,Col("sigma",			'float')
				,Col("nbands"					)
				,Col("mixing",			'float',)
				,Col("nmix"					)
				,Col("xtol",			'float',)
				,Col("strain",			'float',)
				,Col("gga",				'str',	)
				,Col("luse_vdw"				)
				,Col("zab_vdw",			'float',)
				,Col("nelmdl"					)
				,Col("gamma"					)
				,Col("dipol",			'str',	)
				,Col("algo",			'str',	)
				,Col("ibrion"					)
				,Col("prec",			'str',	)
				,Col("ionic_steps")
				,Col("lreal",			'str'	)
				,Col("lvhar"					)
				,Col("diag",			'str'	)
				,Col("spinpol"					)
				,Col("dipole"				)
				,Col("maxstep"					)
				,Col("delta",			'float')
				,Col("mixingtype",		'str'	)
				,Col("bonded_inds",		'str'	)
				,Col("energy_cut_off",	'float')
				,Col("step_size",		'float')
				,Col("spring",			'float')])

	##############################
	,Table('traj'
		,desc = 'A step in a relaxation'
		,cols = [Col('job_id',pk=True)
			 	,Col('step',pk=True)
				,Col('energy','float')
				,Col('is_init',nn=True)
				,Col('is_final',nn=True)]
		,fks = [FK('job_id','relax_job','job_id')])

	,Table('finaltraj'
		,desc = "Table to point to the final step of a trajectory"
		,cols = [Col('traj_job',pk=True)
				,Col('traj_step',pk=True)]
		,fks  = [FK(['traj_job','traj_step'],'traj',['job_id','step'])])

	,Table('trajatom'
		,desc = "An atom considered as part of an optimization trajectory"
		,cols = [Col('traj_job',pk=True)
				,Col('traj_step',pk=True)
				,Col('structure_id',pk=True)
				,Col('ase_id',pk=True)
				,Col('fx','float')
				,Col('fy','float')
				,Col('fz','float')]
		,fks  = [FK(['traj_job','traj_step'],'traj',['job_id','step'])
				,FK(['structure_id','ase_id'],'atom',['structure_id','ase_id'])])

	,Table('structure' # ADD ENERGY AS COLUMN
		,desc = 'List of unique combinations of [atom] and cell'
		,cols = [Col('id',pk=True)
				,Col('cell_id',nn=True)
				,Col('raw_structure','text',nn=True)
				,Col('system_type','str')] # used to partition subclasses
		,fks=[FK('cell_id','cell')])

	,Table('molecule'
		,desc = 'Subclass of structure'
		,cols = [Col('structure_id',pk=True)
				,Col('pointgroup','str')]
		,fks=[FK('structure_id','structure')])

	,Table('surface'
		,desc = 'Subclass of structure'
		,cols = [Col('structure_id',pk=True)
				,Col('vacuum','float')
				,Col('facet_h'),Col('facet_k'),Col('facet_l')]
		,fks=[FK('structure_id','structure')])

	,Table('bulk'
		,desc = 'Subclass of structure'
		,cols = [Col('structure_id',pk=True)
				,Col('spacegroup')]
		,fks=[FK('structure_id','structure')])

	,Table('cell'
		,desc = 'Periodic cells defined by three vectors'
		,cols = [Col('id',pk=True)
				,Col('a0','float',nn=True,uniq=True)
			  	,Col('a1','float',nn=True,uniq=True)
			  	,Col('a2','float',nn=True,uniq=True)
			  	,Col('b0','float',nn=True,uniq=True)
			  	,Col('b1','float',nn=True,uniq=True)
			  	,Col('b2','float',nn=True,uniq=True)
			  	,Col('c0','float',nn=True,uniq=True)
			  	,Col('c1','float',nn=True,uniq=True)
			  	,Col('c2','float',nn=True,uniq=True)
			  	,Col('volume','float')
			  	,Col('surface_area','float')])

	,Table('atom'
		,desc='List of atoms in unique structures'
		,cols = [Col('structure_id',pk=True)
				,Col('ase_id',pk=True)
				,Col('x','float',nn=True)
				,Col('y','float',nn=True)
				,Col('z','float',nn=True)
				,Col('constrained'),Col('magmom','float')]
		,fks=[FK('structure_id','structure','id')
			 ,FK('element_id','element','id')])

    ,Table('composition'
		,desc = 'Chemical Composition'
		,cols = [Col('job_id',pk=True)
			  	,Col('element_id',pk=True)
				,Col('has',nn=True)
				,Col('has_const',nn=True)
				,Col('count',nn=True)
				,Col('count_const',nn=True)
				,Col('count_norm',nn=True)
				,Col('frac',nn=True)
				,Col('frac_const',nn=True)
				,Col('frac_norm',nn=True)
				]
		,fks = [FK('job_id','relax_job','job_id')
			   ,FK('element_id','element','id')])

	,Table('element'
		,desc = 'ID = atomic number' # table for storing data in element.json
		,cols = [Col('id',pk=True)
				,Col('symb','str',nn=True)
				,Col('mass','float',nn=True)])

	,Table('stoich'
		,desc = 'Mapping table between rxn and relaxed_structure'
		,cols = [Col('job_id',pk=True)
				,Col('step',pk=True)
				,Col('rxn_id',pk=True)
				,Col('coef',nn=True)]
		,fks = [FK('job_id','finaltraj','traj_job')
				,FK('step','finaltraj','traj_step')
				,FK('rxn_id','rxn')])
	,Table('rxn'
		,desc = 'Any reaction'
		,cols = [Col('id',pk=True)
				,Col('rxn_name','str')])
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
		,query    = ('select CAST(relax_job.job_id as CHAR),job.stordir '
					+'from relax_job join job on relax_job.job_id = job.id')
		,intype   = ['Str']
		,template = Temp(tempfuncs = [TempFunc(name = 'parse_dftcode'
											  ,args  = [TFA('get_files')])

									 ,TempFunc(name = 'parse_xc'
							 				  ,args  = [TFA('get_files')
											   		   ,TFA('parse_dftcode')])

									 ,TempFunc(name = 'parse_pw'
							 				  ,args   = [TFA('get_files')
											   		    ,TFA('parse_dftcode')])

									 ,TempFunc(name = 'parse_psp'
							 				  ,args   = [TFA('get_files')
											   		    ,TFA('parse_dftcode')])

								     ,TempFunc(name = 'get_files'
									  	      ,args   = [STDIN(0)])]
					   ,stdout = [TFA(source = 'parse_dftcode')
					   			 ,TFA(source = 'parse_xc')
					   			 ,TFA(source = 'parse_pw')
					   			 ,TFA(source = 'parse_psp')])
		,fk_tab = "relax_job"
		,fk_col = "calc_id")

	,Transform(name='populate_relax_calc_other'
		,ttype    = 'Upsert'
		,table    = 'calc_other'
		,cols     = calc_cols
		,outtype  = calc_types
		,query    = ('select cast(relax_job.job_id as CHAR),job.stordir,job.paramdict,calc.dftcode '
		 			+'from relax_job join job on relax_job.job_id = job.id '
					+' join calc on calc.id = relax_job.calc_id')
		,intype   = ['Str','Str','Str']
		,template = Temp(tempfuncs = [TempFunc(name = 'get_files' # type: ignore
									  	      ,args   = [STDIN(0)])

									,TempFunc(name= 'get_vasp_key' # 'get_prec'
											 ,id    = 'prec'
											 ,args  = [STDIN(2) # dftcode
											 		  ,TFA('get_files')
													  ,TFC("'PREC'")
													  ,TFC("'str'")])

									,TempFunc(name= 'get_vasp_key' # 'get_luse_vdw'
											 ,id    = 'luse_vdw'
											 ,args  = [STDIN(2) # dftcode
											 		  ,TFA('get_files')
													  ,TFC("'LUSE_VDW'")
													  ,TFC("'bool'")])

									,TempFunc(name= 'get_vasp_key' # 'get_luse_vdw'
											 ,id    = 'gga'
											 ,args  = [STDIN(2) # dftcode
											 		  ,TFA('get_files')
													  ,TFC("'GGA'")
													  ,TFC("'bool'")])

									,TempFunc(name= 'get_vasp_key' # 'get_lvhar'
											 ,id    = 'lvhar'
											 ,args  = [STDIN(2) # dftcode
											 		  ,TFA('get_files')
													  ,TFC("'LVHAR'")
													  ,TFC("'bool'")])

									,TempFunc(name= 'get_vasp_key' # 'get_zab_vdw'
											 ,id    = 'zab_vdw'
											 ,args  = [STDIN(2) # dftcode
											 		  ,TFA('get_files')
													  ,TFC("'ZAB_VDW'")
													  ,TFC("'float'")])

									,TempFunc(name= 'get_vasp_key' # 'get_algo'
											 ,id    = 'algo'
											 ,args  = [STDIN(2) # dftcode
											 		  ,TFA('get_files')
													  ,TFC("'ALGO'")
													  ,TFC("'str'")])

									,TempFunc(name= 'get_vasp_key' # 'get_nelmdl'
											 ,id    = 'nelmdl'
											 ,args  = [STDIN(2) # dftcode
											 		  ,TFA('get_files')
													  ,TFC("'NELMDL'")
													  ,TFC("'int'")])

									,TempFunc(name= 'get_vasp_key' # 'get_ibrion'
											 ,id    = 'ibrion'
											 ,args  = [STDIN(2) # dftcode
											 		  ,TFA('get_files')
													  ,TFC("'IBRION'")
													  ,TFC("'int'")])

									,TempFunc(name= 'get_vasp_key' # 'get_lreal'
											 ,id    = 'lreal'
											 ,args  = [STDIN(2) # dftcode
											 		  ,TFA('get_files')
													  ,TFC("'LREAL'")
													  ,TFC("'str'")])

									,TempFunc(name= 'get_vasp_key' # 'get_ionic_steps'
											 ,id    = 'ionic_steps'
											 ,args  = [STDIN(2) # dftcode
											 		  ,TFA('get_files')
													  ,TFC("'NSW'")
													  ,TFC("'int'")])

									,TempFunc(name='get_dipol'
											 ,args  = [STDIN(2)
											 		  ,TFA('get_files')])

									,TempFunc(name='get_gamma'
											 ,args  = [STDIN(2)
											 		  ,TFA('get_files')])

									,TempFunc(name='get_econv'
											 ,args  = [STDIN(2)
											 		  ,TFA('get_files')])

									,TempFunc(name='get_dw'
											 ,args  = [STDIN(2)
											 		  ,TFA('get_files')])

									,TempFunc(name='get_sigma'
											 ,args  = [STDIN(2)
											 		  ,TFA('get_files')])

									,TempFunc(name='get_mixing'
											 ,args  = [STDIN(2)
											 		  ,TFA('get_files')])

									,TempFunc(name='get_nbands'
											 ,args  = [STDIN(2)
											 		  ,STDIN(1)
											 		  ,TFA('get_files')])

									,TempFunc(name='get_mixtype'
											 ,args  = [STDIN(2)
											 		  ,TFA('get_files')])

									,TempFunc(name='get_nmix'
											 ,args  = [STDIN(2)
											 		  ,TFA('get_files')])

									,TempFunc(name='get_diag'
											 ,args  = [STDIN(2)
											 		  ,TFA('get_files')])

									,TempFunc(name='get_spinpol'
											 ,args  = [STDIN(2)
											 		  ,TFA('get_files')])

									,TempFunc(name='get_dipole'
											 ,args  = [STDIN(2)
											 		  ,TFA('get_files')])

									,TempFunc(name='get_maxstep'
											 ,args  = [STDIN(2)
											 		  ,TFA('get_files')])

									,TempFunc(name="get"
											 ,id    = 'fmax'
											 ,args  = [STDIN(1)
											 		  ,TFC("'fmax'")
													  ,TFC("'float'")])

									,TempFunc(name="get"
											 ,id    = 'xtol'
											 ,args  = [STDIN(1)
											 		  ,TFC("'xtol'")
													  ,TFC("'float'")])

									,TempFunc(name="get"
											 ,id    = 'strain'
											 ,args  = [STDIN(1)
											 		  ,TFC("'strain'")
													  ,TFC("'float'")])

									,TempFunc(name="get"
											 ,id    = 'energy_cut_off'
											 ,args  = [STDIN(1)
											 		  ,TFC("'energy_cut_off'")
													  ,TFC("'float'")])

									,TempFunc(name="get"
											 ,id    = 'bonded_inds'
											 ,args  = [STDIN(1)
											 		  ,TFC("'bonded_inds'")
													  ,TFC("'str'")])

									,TempFunc(name="get"
											 ,id    = 'step_size'
											 ,args  = [STDIN(1)
											 		  ,TFC("'step_size'")
													  ,TFC("'float'")])

									,TempFunc(name="get"
											 ,id    = 'spring'
											 ,args  = [STDIN(1)
											 		  ,TFC("'spring'")
													  ,TFC("'float'")])

									,TempFunc(name="get"
											 ,id    = 'delta'
											 ,args  = [STDIN(1)
											 		  ,TFC("'delta'")
													  ,TFC("'float'")])

									,TempFunc(name="get_kpts"
											 ,args  = [STDIN(2)
											 		  ,TFA('get_files')])

									]

					   ,stdout = calc_tfas)
		,fk_tab = "relax_job"
		,fk_col = "calc_other_id")
	#
	# ,Transform(name = 'populate_cell'
	# 	,ttype 	  = 'Upsert'
	# 	,table 	  = 'cell'
	# 	,cols 	  = ['a0','a1','a2','b0','b1','b2','c0','c1','c2']
	# 	,outtype  = ['Float']*9
	# 	,query    = 'select structure.id,structure.raw_structure from structure'
	# 	,intype   = ['Str']
	# 	,template = SimpleTemp('extract_cell',n_outputs=9)
	# 	,fk_tab   = 'structure'
	# 	,fk_col   = 'cell_id')

	,Transform(name='get_job_metadata'
		,ttype   = 'Update'
		,table   = 'job'
		,cols    = ['user','timestamp','working_directory'
				   ,'storage_directory','paramdict','job_type']
		,outtype = ['Str','Int','Str'
				   ,'Str','Str','Str']
		,query   = "select cast(job.id as CHAR),job.stordir from job"
		,intype  = ['Str']
		,template = SimpleTemp('get_job_metadata',n_outputs=6))

		]
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
	# 										  ,args   = [STDIN(0) # id
	# 										  		    ,TFA('get_traj',id='init') # ase.Atoms
	# 												    ,TFA('get_traj',id='final') # ase.Atoms
	# 												    ,FuncArg('traj_to_json')])
	# 								,TempFunc(name = 'get_traj'
	# 										 ,id     = 'init'
	# 										 ,args   = [STDIN(1) # path
	# 												   ,TFC("'init'")])
	# 								,TempFunc(name = 'get_traj'
	# 										 ,id     = 'final'
	# 										 ,args   = [STDIN(1) # path
	# 												   ,TFC("'final'")])
	# 								 ]
	# 				   ,stdout = [TFA(source = 'get_init_final_structure')]))
