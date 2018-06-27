import typing as typ
from os.path import join

def get_fwid(path :str)->int:
	"""
	Get Fireworks ID:

	Looks for a particular FW_submit.script file, characteristic of FW jobs.
	"""

	try:
		with open(join(path,'FW_submit.script'),'r') as f: fw = f.readlines()
		for line in fw:
			if '--fw_id' in line:
				return int(line.split()[-1])

		raise ValueError('No fw_id found in FW_submit.script: \n\n%s'%fw)
	except IOError:
		return -1
