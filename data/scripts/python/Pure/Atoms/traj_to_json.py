import ase # type: ignore
import json

def traj_to_json(atoms : ase.Atoms) -> str:
	"""
	Serialize an Atoms object in a human readable way
	"""

	def rounder(y : float)->float:
		return round(y,3) + 0 #+0 turns -0 into 0

	atomdata = []
	const    = atoms.constraints
	if const: const_list = const[0].get_indices().tolist()
	else:     const_list = []
	for a in atoms: atomdata.append({'number'		: int(a.number)
									,'x'			: rounder(a.x)
									,'y'			: rounder(a.y)
									,'z'			: rounder(a.z)
									,'magmom'		: rounder(a.magmom)
									,'tag'			: int(a.tag)
									,'constrained'	: int(a.index in const_list)
									,'index'		: int(a.index)})

	out = {'cell': [[rounder(x) for x in xx] for xx in atoms.get_cell().tolist()]
		  ,'atomdata':atomdata}

	return json.dumps(out)
