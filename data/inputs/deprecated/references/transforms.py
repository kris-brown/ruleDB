import os
from dbgenPython.support.utils import *
###########
# Constants
###########
pypth   = os.environ['DBGEN_ROOT']+'/data/scripts/python/'

######
# Data
######

functions = {
	'get_refeng' 			: pypth+"IO/get_refeng.py"
}


transforms = [
    Transform(name = 'populate_refeng'
    		 ,ttype   = 'Insert'
    		 ,table   = 'refeng'
    		 ,cols    = ['element_id','calc_id']
    		 ,outtype = ['Int','Int']
    		 ,query   = "not implemented"
    		 ,intype  = []
			 ,template = SimpleTemp('get_refeng')
             )

]
