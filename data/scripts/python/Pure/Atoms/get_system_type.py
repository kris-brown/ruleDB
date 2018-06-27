import json
import numpy as np  # type: ignore
import ase  # type: ignore

def get_system_type(atoms : ase.Atoms)->str:
	"""
	Helpful docstring
	"""
	############################################################################
	# Parameters
	#------------
	cutoff    = 6  # A

	# Preprocessing
	atoms.center() # In case dealing with Nerds Rope & Co.
	minx,miny,minz,maxx,maxy,maxz = 1000,1000,1000,-1000,-1000,-1000
	for a in atoms:
		if a.position[0] < minx: minx =  a.position[0]
		if a.position[1] < miny: miny =  a.position[1]
		if a.position[2] < minz: minz =  a.position[2]
		if a.position[0] > maxx: maxx =  a.position[0]
		if a.position[1] > maxy: maxy =  a.position[1]
		if a.position[2] > maxz: maxz =  a.position[2]

	cell_abc  = list(map(np.linalg.norm,atoms.get_cell()[:3]))
	thickness = [maxx-minx,maxy-miny,maxz-minz]
	pbc       = [cell_abc[i]-thickness[i] < cutoff for i in range(3)]

	if   all(pbc): return 'bulk'
	elif any(pbc): return 'surface'
	else:          return 'molecule'
