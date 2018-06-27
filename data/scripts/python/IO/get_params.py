def get_params(path : str) -> str:
	"""
	Grab the parameters dictionary but throw away any pickle strings
	Something is also broken for jsonified strings of dictionaries as values
	so we explicitly remove 'job_name' and 'project' keys. Not sure how to fix
	this (or even have a better general way of throwing out jsond dictionaries)
	"""
	import json
	from os.path import join
	#########################################################################
	with open(join(path,'params.json'),'r') as f:
		p = json.load(f)

	for key,val in p.copy().items():
		try:
			if '\\x00' in val or key in ['kwargs','project']:
				del p[key]
		except TypeError:
			pass
	return json.dumps(p)
