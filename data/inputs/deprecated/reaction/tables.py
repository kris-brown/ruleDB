from dbgenPython.support.utils import Table

"""


"""

tables = [
	Table('stoich'
        ,'A many-many mapping table between relax_job and reaction'
		,nnull=['job_id','rxn_id','stoich']
		,fks  = {
				'job_id':'relax_job'
				,'rxn_id':'rxn'
				}
        ,uniqs =[['raw_state']])

    ,Table('state_component'
        ,'A component of a chemical state'
		,nnull=['state_id','job_id','stoich']
		,fks = {'state_id':'state'
			   ,'job_id': 'relax_job'}
		,uniqs=[['state_id','job_id']])

    ,Table('rxn'
        ,'A pair of states with the same composition'
		,nnull = ['reactants'
				 ,'products'
				 ,'delta_h'
				 ,'is_formation'
				 ,'is_adsorption']
		,fks   = {'reactants' : 'state'
				 ,'products'  : 'state'}
        ,uniqs = [['reactants','products']])

    ,Table('rxn_w_barrier'
        ,'A reaction with a calculated barrier'
		,nnull	= ['rxn_id','barrier','activation_energy']
		,fks	= {'rxn_id':'rxn','barrier':'state'}
		,uniqs	= [['rxn_id','barrier']])

    ,Table('rxn_network_component'
        ,'A reaction in a reaction network'
		,nnull = ['rxn_network_id','rxn_w_barrier_id']
		,fks   = {'rxn_network_id'   : 'rxn_network'
				 ,'rxn_w_barrier_id' : 'rxn_w_barrier'}
		,uniqs = [['rxn_network_id','rxn_w_barrier_id']])

    ,Table('rxn_network'
        ,'A set of reaction with well-defined macroscopic behavior'
		,nnull=['raw_rxn_network','n_rxns','n_species']
		,uniqs=[['raw_rxn_network']])
    ]
