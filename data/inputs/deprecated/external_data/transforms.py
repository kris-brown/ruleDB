import os
from dbgenPython.support.utils import *
###########
# Constants
############
pypth   = os.environ['DBGEN_ROOT']+'/data/scripts/python/'
#########
# Data
#########

functions = {
	'parse_keld' 			: pypth+'IO/parse_keld.py'
	,'parse_mendeleev' 		: pypth+'IO/parse_mendeleev.py'
}
##############################################
transforms = [Transform(name = 'populate_keld'
		,ttype = 'Insert'
		,table = 'keld_solids'
		,cols  = ['name'
				 ,'lattice_parameter'
				 ,'cohesive_energy'
				 ,'bulk_modulus'
				 ,'structure'
				 ,'magmom']
		,outtype= ['Str'
				  ,'Float'
				  ,Maybe('Float')
				  ,Maybe('Float')
				  ,'Str'
				  ,Maybe('Float')]
		,query = None
		,template= SimpleTemp('parse_keld',n_inputs=0))

	# ,Transform(name='populate_mendeleev'
	# 	,ttype ='Update'
	# 	,table = 'element'
	# 	,cols = ['atomic_mass','reference_spacegroup','reference_pointgroup']
	# 	,outtype = ['Float',Maybe('Int'),Maybe('Str')]
	# 	,query= 'select element.id,element.id from element'
	# 	,intype = ["Int"]
	# 	,template= SimpleTemp('parse_mendeleev',n_outputs=3))

]
