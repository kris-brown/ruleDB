import typing as typ
import os
from os.path import join,exists

def get_dft_logfiles() -> typ.List[str]:
	"""
	Searches for DFT calculation folders
	"""
	############################
	output = []
	rootpath = os.environ['DBGEN_ROOT']

	for root, _, _ in os.walk(rootpath):
		if exists(join(root,'runtime.json')):
			output.append(root)

	return output # in general case an INSERT can insert many things.
