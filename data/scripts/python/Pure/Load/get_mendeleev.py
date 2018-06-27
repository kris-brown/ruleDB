import typing as typ
import os

def get_mendeleev()-> typ.List[str]:
	"""
	Returns file path for Mendeleev elemental data
	"""
	home = os.environ['HOME']

	return [home + '/dbgen/data/element.json']
