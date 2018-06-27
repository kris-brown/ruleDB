import typing as typ
import os,imp

def parse_keld() -> typ.List[typ.Tuple[str,float,float,float,str,float]]:
	"""
	Extracts information of materials
	"""

	rootpath = os.environ['DBGEN_ROOT']
	keld = imp.load_source('keld',os.path.join(rootpath,'data/data_solids_wPBE.py'))

	cols = ['name'
			,'lattice parameter'
			,'cohesive energy'
			,'bulk modulus'
			,'structure'
			,'magmom']

	def xtract(d):
		return tuple(map(d.get,cols)) # type: ignore

	return [xtract(x) for x in  keld.data.values()]  # type: ignore
