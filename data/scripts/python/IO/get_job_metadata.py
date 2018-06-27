import typing as typ
import json
from os.path import join

def get_job_metadata(stordir : str
					) -> typ.Tuple[str,int,str,str,str,str]:
	"""
	Takes a path to a DFT calculation and extracts information from runtime.json
	Also keeps a record of where the information was taken from.
	"""
	############################
	with open(join(stordir,"runtime.json"),'r') as f:
		runtime = json.load(f)

	############################
	with open(join(stordir,"params.json"),'r') as f:
		params = f.read()


	def isnt_binary(x):
		"""THIS DOESN'T WORK WHY?"""
		return not (isinstance(x,str) and "\x00" in x)

	raw_pdict = json.loads(params)
	pdict = {k:v for k,v in raw_pdict.items() if 'pckl' not in k and 'project' not in k} # discard binary values

	#############################
	jobkind =  pdict.get('jobkind','<no jobkind>')

	return tuple([runtime.get('user')
				 ,runtime.get('timestamp')
				 ,runtime.get('working_directory')
				 ,runtime.get('storage_directory')
				 ,json.dumps(pdict)
				 ,jobkind
				 ]) # type: ignore
