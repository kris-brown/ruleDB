import os
from dbgenPython.support.utils import *
################################################################################
# Constants
############
pypth   = os.environ['DBGEN_ROOT']+'/data/scripts/python/'

#################################################################################
#########
# Data
#########
functions = {
	'find_states' 		     : pypth+'Pure/Rxns/find_states.py'
	,'get_state_components'  : pypth+'Pure/Rxns/get_state_components.py'
	,'is_reference'			 : pypth+'Pure/Rxns/is_reference.py'
	,'get_rxn' 		       	 : pypth+'Pure/Rxns/get_rxn.py'
    ,'get_deficiency_number' : pypth+'Pure/Rxns/get_deficiency_number.py'
	,'is_linear'	 		 : pypth+'Pure/Rxns/is_linear.py'
	,'get_formula'  		  : pypth+'Pure/Atoms/get_formula.py'

    }
##############################
transforms = [Transform(name = 'populate_state'
            			,ttype   = 'Insert'
            			,table   = 'state'
            			,cols    = ['raw_state','state_h']
						,outtype = ['Str','Float']
            			,query   = 'select * from relax_job'
						,intype  = []
            			,template = SimpleTemp('find_states'))

	,Transform(name = 'populate_state_component'
            			,ttype = 'Insert'
            			,table = 'state_component'
            			,cols  = ['state_id','job_id','stoich']
						,outtype = ['Int',"Int",'Float']
            			,query = 'select state.id,state.raw_state from state'
            			,intype = []
            			,template  = SimpleTemp('get_state_components'))

	,Transform(name='formula'
		,ttype='Update'
		,table='relax_job'
		,cols = ['formula']
		,outtype=['Str']
		,query = ('select structure.job_id,structure.raw_structure '
				 +'from structure where structure.is_init')
		,template = SimpleTemp('get_formula'))


	,Transform(name = 'is_reference'
		,ttype   = 'Update'
		,table   = 'state'
		,cols    = ['reference']
		,outtype = ['Int']
		,query   = ('select state.id,relax_job.formula,structure.pointgroup '
				   		  +',structure.spacegroup from state '
					   +'join state_component '
						   +'on state.id = state_component.state_id '
					   +'join relax_job '
						   +'on state_component.job_id = relax_job.job_id '
					   +'join structure '
					   	  +'on structure.job_id = relax_job.job_id '
					   +'where structure.is_final')
		,intype = ['Str','Str','Int']
		,template = SimpleTemp('is_reference',n_inputs=3)
		)

    ,Transform(name = 'populate_rxn'
		,ttype = 'Insert'
		,table = 'rxn'
		,cols  = ['reactants','products','delta_h','is_formation','is_adsorption']
		,outtype=['Int','Int','Float','Int',"Int"]
		,query = 'select * from state'
		,intype = []
		,template  = SimpleTemp('get_rxn'))

    ,Transform(name = 'populate_rxn_w_barrier'
		,ttype 	 = 'Insert'
		,table	 = 'rxn_w_barrier'
		,cols  	 = ['rxn_id','barrier','activation_energy']
		,outtype = ['Int','Int','Float']
		,query 	 = 'select * from neb_job join rxn on 1')

    ,Transform(name = 'populate_rxn_network_component'
		,ttype = 'Insert'
		,table = 'rxn_network_component'
		,cols  = ['rxn_network_id','rxn_w_barrier_id']
		,outtype= ['Int','Int']
		,query = 'select * from rxn_network')

    ,Transform(name = 'populate_rxn_network'
		,ttype = 'Insert'
		,table = 'rxn_network'
		,cols  = ['raw_rxn_network','n_rxns','n_species']
		,outtype= ['Int','Int','Int']
		,query = 'select * from rxn_w_barrier')

    ,Transform(name = 'deficiency_number'
		,ttype = 'Update'
		,table = 'rxn_network'
		,cols  = ['deficiency_number']
		,outtype = ['Int']
		,query = 'select rxn_network.id from rxn_network'
		,intype = []
		,template  = SimpleTemp('get_deficiency_number'))

]
