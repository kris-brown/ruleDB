import typing as typ
from os.path import join
from dbgenPython.core.parsing import parse_line

def get_eng(storage_directory	: str
		   ,dftcode				: str
		   ) -> float:
	"""
	Parses log file to get the raw_energy (extrapolated to 0 K) of a relax job
	"""
	def readFile(x:str)->str:
		with open(join(storage_directory,x),'r') as f:
			return f.read()

	def getENG_GPAW(log:str)->float:
		return float(parse_line(log,'Extrapolated:',-1).split()[-1])
	def getENG_QE(log:str)->float:
		e =  float(parse_line(log,'total energy',-2).split()[-2])
		ts = float(parse_line(log,'smearing contrib.',-1).split()[-2])
		return (e-ts/2)*13.6057
	def getENG_VASP(log:str)->float:
		return float(parse_line(log,'energy(sigma->0)',-1).split()[-1])

	if   dftcode == 'gpaw':
		return getENG_GPAW(readFile('log'))
	elif dftcode == 'quantumespresso':
		return getENG_QE(readFile('log'))
	elif dftcode == 'vasp':
		return getENG_VASP(readFile('OUTCAR'))
	else:
		raise NotImplementedError(dftcode)
