from dbgenPython.support.utils import Table,FK,Col

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
		,cols = [Col('id',pk=True)
				,Col("raw_calc_other",'str',nn=True,uniq=True)
				,Col("kx"),Col("ky"),Col("kz")
				,Col("fmax",'float')
				,Col("econv",'float')
				,Col("dw")
				,Col("sigma",'float')
				,Col("nbands")
				,Col("mixing",'float')
				,Col("nmix")
				,Col("xtol",'float')
				,Col("strain",'float')
				,Col("gga",'str')
				,Col("luse_vdw")
				,Col("zab_vdw",'float')
				,Col("nelmdl")
				,Col("gamma")
				,Col("dipol",'str')
				,Col("algo",'str')
				,Col("ibrion")
				,Col("prec",'str')
				,Col("ionic_steps")
				,Col("lreal",'str')
				,Col("lvhar")
				,Col("diag",'str')
				,Col("spinpol")
				,Col("dipole")
				,Col("maxstep")
				,Col("delta",'float')
				,Col("mixingtype",'str')
				,Col("bonded_inds",'str')
				,Col("energy_cut_off",'float')
				,Col("step_size",'float')
				,Col("spring",'float')])

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
		,fks  = [FK('traj_job','traj','job_id')
			    ,FK('traj_step','traj','step')])

	,Table('trajatom'
		,desc = "An atom considered as part of an optimization trajectory"
		,cols = [Col('traj_job',pk=True)
				,Col('traj_step',pk=True)
				,Col('atom_ind',pk=True)
				,Col('fx','float')
				,Col('fy','float')
				,Col('fz','float')]
		,fks  = [FK(['traj_job','traj_step'],'traj',['job_id','step'])
				,FK(['atom_ind','structure_id'],'atom',['ind','structure_id'])])

	,Table('structure' # ADD ENERGY AS COLUMN
		,desc = 'List of unique combinations of [atom] and cell'
		,cols = [Col('id',pk=True)
				,Col('raw_structure','str',nn=True,uniq=True)
				,Col('system_type','str')] # used to partition subclasses
		,fks=[FK('cell_id','cell')])

	,Table('molecule'
		,desc = 'Subclass of structure'
		,cols = [Col('structure_id',pk=True)
				,Col('pointgroup','str')
				]
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
				,Col('spacegroup')
				]
		,fks=[FK('structure_id','structure')])

	,Table('cell'
		,desc = 'Periodic cells defined by three vectors'
		,cols = [Col('id',pk=True)
				,Col('ax','float',nn=True,uniq=True)
			  	,Col('ay','float',nn=True,uniq=True)
			  	,Col('az','float',nn=True,uniq=True)
			  	,Col('bx','float',nn=True,uniq=True)
			  	,Col('by','float',nn=True,uniq=True)
			  	,Col('bz','float',nn=True,uniq=True)
			  	,Col('cx','float',nn=True,uniq=True)
			  	,Col('cy','float',nn=True,uniq=True)
			  	,Col('cz','float',nn=True,uniq=True)
			  	,Col('volume','float')
			  	,Col('surface_area','float')])

	,Table('atom'
		,desc='List of atoms in unique structures'
		,cols = [Col('structure_id',pk=True)
				,Col('ind',pk=True)
				,Col('x','float',nn=True)
				,Col('y','float',nn=True)
				,Col('z','float',nn=True)
				,Col('constrained'),Col('magmom','float')]
		,fks=[FK('structure_id','structure')
			 ,FK('element_id','element')])

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
			   ,FK('element_id','element')])

	,Table('element'
		,desc = 'ID = atomic number' # table for storing data in element.json
		,cols = [Col('id',pk=True)
				,Col('symb','str',nn=True)
				,Col('mass','float',nn=True)
				])

	,Table('stoich'
		,desc = 'Mapping table between rxn and relaxed_structure'
		,cols = [Col('job_id',pk=True)
				,Col('step',pk=True)
				,Col('rxn_id',pk=True)
				,Col('coef',nn=True)]
		,fks = [FK(['job_id','step'],'finaltraj',['traj_job','traj_step'])
				,FK('rxn_id','rxn')])
	,Table('rxn'
		,desc = 'Any reaction'
		,cols = [Col('id',pk=True)
				,Col('rxn_name','str')])
]
