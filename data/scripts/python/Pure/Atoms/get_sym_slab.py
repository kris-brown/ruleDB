import ase # type: ignore

def get_sym_slab(atoms:ase.Atoms)->int:
	"""
	Crude check if slab PROBABLY is symmetric

	Really just checks whether or not the constrained atoms are in the middle
	"""

	raise NotImplementedError
