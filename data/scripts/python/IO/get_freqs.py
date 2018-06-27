import typing as typ
def get_freqs(job_id:int
			 ,stordir:str
			 )->typ.List[typ.Tuple[int,float,int]]:
	"""
	Returns real and imaginary frequencies from a vibrational calculation

	Currently not implemeneted (serves as a test case for transforms that fail)
	"""
	real,imag = '',''
	mags=[]
	raise NotImplementedError
	return [(job_id,mag,mag>0) for mag in mags]
