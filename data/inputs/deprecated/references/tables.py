from dbgenPython.support.utils import Table

tables = [
    	Table('refeng'
    		,'Convenient storage of elemental reference energies at given calculators. One row per reference calculation'
            ,nnull=['element_id','calc_id']
            ,fks={'element_id':'element','calc_id':'calc'}
    		,uniqs=[['element_id','calc_id']])

    ]
