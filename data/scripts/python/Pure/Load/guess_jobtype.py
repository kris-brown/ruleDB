import typing as typ

def guess_jobtype(files : typ.Dict[str,str]) -> str:
    """
    Decide whether job is a vib job, a collection of singlepoints (e.g. bulkmod/latticeopt), or a relax
    """
    vibpckls = files['vibpckls']
    qnlog    = files['qnlog']
    if len(vibpckls) > 0:                                  return 'vib'          # for vib, read INITIAL atoms from any .traj file (final doesn't matter)
    else:
        bfgs = qnlog.count('BFGS:')                                              # for bulkmod or lattice opt, read INITIAL atoms from any .traj file (accuracy here matters for bulkmod, not latticeopt)
        if bfgs > 1 and bfgs==qnlog.count('BFGS:    0'):   return 'singlepoints' # possibly relax converges with single point, exclude these jobs (bulkmods and latticeopts take more than one singlepoint)
        else:                                              return 'relax'        # includes vc-relax
