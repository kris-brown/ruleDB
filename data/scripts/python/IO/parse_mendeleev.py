import typing as typ
import json
import os

def parse_mendeleev(ind : int
				   )->typ.Tuple[int,str,float,int,str]:
	"""
	Extracts information of elements
	"""
	rootpath = os.environ['DBGEN_ROOT']

	with open(os.path.join(rootpath,'data/element.json'),'r') as f:
		elem = json.load(f)[ind]
	############################
	cols = ['atomic_weight','spacegroup','pointgroup'] # etc

	return tuple([elem.get(k) for k in cols]) # type: ignore
