import json
import ase                                  # type: ignore
import pymatgen.io.ase            as pmgase # type: ignore
import  pymatgen.symmetry.analyzer as pysym # type: ignore

def get_spacegroup(atoms : ase.Atoms)->int:
	"""
    uses pymatgen SpacegroupAnalyzer
	"""
	pmg = pmgase.AseAtomsAdaptor().get_structure(atoms)
	sg  = pysym.SpacegroupAnalyzer(pmg,symprec=0.1)
	return sg.get_space_group_number()
