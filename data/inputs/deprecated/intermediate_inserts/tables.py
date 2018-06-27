from dbgenPython.support.utils import Table

"""
This is geared towards partitioning all jobs into bulkmod,relax,etc.

It's intended for 'vib_job' (for example) to be populated directly from "job"
and for the connection between a vib_job and a relax_job to be derived later

An alternative method would be to instantiate 'vib_job' directly from 'relax_job'
(as to enforce that every vib_job is associated with a relax job)

"""

tables = [Table('atom'
		,'List of atoms in unique structures'
		,nnull=['structure_id','ind','element_id','x','y','z','constrained','magmom']
		,fks={'structure_id':'structure'
			,'element_id':'element'}
		,uniqs=[['structure_id','ind']])
	###########################################################

	,Table('bulkmod_job'
		,'Jobs that compute bulk modulus for a relaxed structure'
		,nnull	= ['job_id']
		,fks	= {'job_id':'job'}
		,uniqs	= [['job_id']])
	###########################################################

	,Table('neb_job'
		,'Jobs that compute nebs between two relaxed structures'
		,nnull	= ['job_id']
		,fks	= {'job_id':'job'
			 	  ,'init' : 'relax_job'
			 	  ,'final': 'relax_job'}
		,uniqs	= [['job_id']])


	###########################################################
	,Table('vib_job'
		,'Jobs that compute vibrational modes for relaxed structures'
		,nnull=['job_id']
		,fks={'job_id':'job'}
		,uniqs=[['job_id']])

	,Table('vibmode'
		  ,'Vibrational modes'
		  ,nnull=['vib_job_id','vib_ind','magnitude','real']
		  ,fks={'vib_job_id':'vib_job'}
		  ,uniqs=[['vib_job_id','vib_ind']])
	###########################################################
    ,Table('composition'
		,'Chemical Composition'
		,nnull = ['structure_id','element_id','has']
		,fks={'structure_id' : 'structure'
			 ,'element_id'   : 'element'}
		,uniqs=[['structure_id'
				,'element_id']])

	,Table('element'
		,'ID = atomic number' # table for storing data in element.json
		,nnull=['symbol']
		,uniqs=[['symbol']])

]
