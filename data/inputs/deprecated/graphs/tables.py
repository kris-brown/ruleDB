from dbgenPython.support.utils import Table

"""
We need a transform that will link a given chargemoljob to a relax_job
(if any exists)

"""

tables = [
	Table('chargemol_job'
		,'Jobs that compute bond orders'
		,nnull = ['job_id']
		,fks = {'job_id':'job'}
		,uniqs = [['job_id']])

,Table('bond'
		,"Bonds identified by ChargeMol (T Manz)"
		,nnull = ['chargemol_job_id','bond_ind','ind1','ind2','bondorder']
		,fks   = {'chargemol_job_id':'chargemol_job'}
		,uniqs =[['chargemol_job_id','bond_ind']]
		)
,Table('chargemol_atom'
	,'per-atom properties associated with a given chargemol job'
	,nnull = ['chargemol_job_id','ind','charge']
	,fks   = {'chargemol_job_id':'chargemol_job'}
	,uniqs = [['chargemol_job_id']]
	)
	]
