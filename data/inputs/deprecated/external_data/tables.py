from dbgenPython.support.utils import Table,Col

tables = [
	Table('keld_solids'
		,desc = "List of structures for which we have materials properties"
		,cols = [Col('id',pk=True)
				,Col('name','str',nn=True,uniq=True)
				,Col('lattice_parameter',nn=True)
				,Col('cohesive_energy','float')
				,Col('bulk_modulus','float')
				,Col('structure','str')
				,Col('magmom','float')])

]
