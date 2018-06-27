import typing as typ
import os
def get_keld_data_solids() -> typ.List[str]:
	"""
	Returns file path for keld solids data
	"""
	home = os.environ['HOME']

	return [home + '/dbgen/data/data_solids_wPBE.py']
