import json
from os.path import join

def get_job_name(stordir : str) -> str:
	"""
	Gets job name
	"""

	with open(join(stordir,"runtime.json")) as f:
		runtime = json.load(f)

	name = runtime.get('kwargs')

	try:
		name = json.loads(name)['job_name']
	except:
		pass

	return name
