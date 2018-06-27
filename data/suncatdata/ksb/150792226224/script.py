

def initialize():
    """
    Does three things to initialize (any) job:
        1. Delete old error and output files for restarted jobs
        2. Load job input parameters
        3. Write initial Atoms object to init.traj
    """
    import json,os,glob
    import ase.io as aseio
    import ase.parallel as asepar

    def keepNewest(string):                 # Step 1
        listPth = glob.glob(string)
        ordered = sorted([(os.path.getmtime(pth),pth) for pth in listPth])
        for t,pth in ordered[:-1]: 
            os.remove(pth)

    if rank()==0:
        keepNewest('*.error')
        keepNewest('*.out')

    try:   os.remove('result.json')
    except OSError: pass
    try:   os.remove('runtime.json')
    except OSError: pass

    with asepar.paropen('params.json','r') as f: prms  = json.loads(f.read())    # Step 2 
    atoms   = makeAtoms(prms)

    aseio.write('init.traj',atoms)         # Step 3

    return prms,atoms


def makeAtoms(params): 
    import cPickle
    return cPickle.loads(str(params['inittraj_pckl']))


def makeCalc(params):
    dftcode = params['dftcode']
    jobkind = params['jobkind']
    relax   = jobkind in ['latticeopt','relax','vcrelax']
    kpt     = makeKPT(params)
    spinpol = makeSpinpol(params)
    def makeGPAWcalc(p):
        from gpaw import GPAW,PW,Davidson,Mixer,MixerSum,FermiDirac
        return GPAW(mode         = PW(p['pw'])                        
                    ,xc          = p['xc']
                    ,kpts        = kpt
                    ,spinpol     = spinpol
                    ,convergence = {'energy':p['econv']} #eV/electron
                    ,mixer       = ((MixerSum(beta=p['mixing'],nmaxold=p['nmix'],weight=100)) 
                                    if spinpol else (Mixer(beta=p['mixing'],nmaxold=p['nmix'],weight=100)))
                    ,maxiter       = p['maxstep']
                    ,nbands        = p['nbands']
                    ,occupations   = FermiDirac(p['sigma'])
                    ,setups        = p['psp']
                    ,eigensolver   = Davidson(5)
                    ,poissonsolver = None 
                    ,txt           ='log'
                    ,symmetry={'do_not_symmetrize_the_density': True}) 

    def makeQEcalc(p):
        from espresso import espresso   

        pspDict =   {'sherlock': {'gbrv15pbe':'/home/vossj/suncat/psp/gbrv1.5pbe'}
                    ,'suncat':   {'gbrv15pbe':'/nfs/slac/g/suncatfs/sw/external/esp-psp/gbrv1.5pbe'}}
        pspPath =  pspDict[getCluster()][params['psp']]

        return espresso( pw         = p['pw']
                        ,dw         = p['pw']*p['dwrat']
                        ,xc         = p['xc']
                        ,kpts       = kpt
                        ,spinpol    = spinpol
                        ,convergence=   {'energy':  p['econv']
                                        ,'mixing':  p['mixing']
                                        ,'nmix':    p['nmix']
                                        ,'maxsteps':p['maxstep']
                                        ,'diag':    'david'}
                        ,nbands     = p['nbands']
                        ,sigma      = p['sigma']
                        ,dipole     = {'status': p['kind'] == 'surface'}
                        ,outdir     = 'calcdir'
                        ,startingwfc= 'atomic+random' 
                        ,psppath    = pspPath
                        ,mode       = 'vc-relax' if jobkind=='vcrelax' else 'scf'
                        ,cell_factor= 2 if jobkind == 'vcrelax' else 1
                        ,output     = {'removesave':True})

    def makeQEvibcalc(params):
        from espresso.vibespresso import vibespresso
        return vibespresso( pw          = p['pw']
                            ,dw         = p['pw']*p['dwrat']
                            ,xc         = p['xc']
                            ,kpts       = kpt
                            ,spinpol    = spinpol
                            ,convergence=   {'energy':  p['econv']
                                            ,'mixing':  p['mixing']
                                            ,'nmix':    p['nmix']
                                            ,'maxsteps':p['maxstep']
                                            ,'diag':    'david'}
                            ,nbands     = p['nbands']
                            ,sigma      = p['sigma']
                            ,dipole     = {'status': p['kind'] == 'surface'}
                            ,outdir     = 'calcdir'  
                            ,startingwfc= 'atomic+random' 
                            ,psppath    = pspPath
                            ,output     = {'removesave':True})

    if dftcode =='gpaw':    
        if relax: return makeGPAWcalc(params)
        else: raise NotImplementedError, 'no GPAW calculator-maker for this kind of job'
    elif dftcode =='quantumespresso': 
        if relax:                   return makeQEcalc(params)
        elif jobkind == 'vib':      return makeQEvibcalc(params)
        else: raise NotImplementedError, 'no QE calculator-maker for this kind of job'


def makeKPT(params):    
    """
    Convert k-point density to Monkhorst-Pack grid size. Values forced to be even numbers.
    Special considerations if modeling molecule/bulk/surface. 
    """
    import math  as m
    import numpy as np

    recipcell,kpts = makeAtoms(params).get_reciprocal_cell(),[]
    for i in range(3):
        k = 2 * 3.14159 * m.sqrt((recipcell[i]**2).sum()) * params['kptden'] 
        kpts.append(2 * int(np.ceil(k / 2)))

    kind = params['kind']
    if   kind=='surface':   return np.array(kpts[:2]+[1])
    elif kind=='molecule':  return np.array([1,1,1])
    else:                   return np.array(kpts)


def makeSpinpol(params):
    magmomsinit = makeAtoms(params).get_initial_magnetic_moments()
    return any([x>0 for x in magmomsinit])


def optimizePos(atoms,calc,fmax):
    import ase.optimize as aseopt

    atoms.set_calculator(calc)
    dyn = aseopt.BFGS(atoms=atoms, logfile='qn.log', trajectory='qn.traj',restart='qn.pckl')
    dyn.run(fmax=fmax)


def trajDetails(atoms):
    """ Returns dictionary summary of an (optimized) Atoms object """
    import cPickle
    import numpy as np

    try: mag = atoms.get_magnetic_moments()
    except: mag = np.array([0]*len(atoms))
    return {'finaltraj_pckl':cPickle.dumps(atoms)
            ,'finalpos_pckl':cPickle.dumps(atoms.get_positions())
            ,'finalcell_pckl':cPickle.dumps(atoms.get_cell())
            ,'finalmagmom_pckl':cPickle.dumps(mag)}


def log(params,optatoms):
    import datalog
    datalog.log(optatoms,job_name=params['name'])


def OptimizeLatticeScript():
    import cPickle,json,os,ase,shutil
    import scipy.optimize as opt
    import ase.parallel   as asepar

    global energies,lparams

    #######################
    print "Initializing..."
    #----------------------
    params,initatoms = initialize()      # Remove old .out/.err files, load from fw_spec, and write 'init.traj'
    shutil.copy('init.traj','out.traj')  # Initial guess for atomic coordinates inside getBulkEnergy reads from 'out.traj'
    energies,lparams = [],[]             # Initialize variables

    if rank()==0:
        for d in ['qn.traj','qn.log','lattice_opt.log','energy_forces.pckl']:          # Remove partially completed calculations that may still be held over from failed job
            if os.path.exists(d): os.remove(d)
            print 'Removed existing file ',d

    ################################
    print "Loading initial guess..."
    #-------------------------------
    
    try:
        with asepar.paropen('lastguess.json','r') as f: iGuess = json.loads(f.read())
        print '\tread lastguess from lastguess.json: ',iGuess
    except: 
        iGuess = getInitGuess(params['structure'],initatoms.get_cell())
        print '\tgenerating initial guess from getInitGuess(): ',iGuess
    
    ########################################
    print "Optimizing cell and positions..."
    #---------------------------------------
    optimizedLatticeParams  = opt.fmin(getBulkEnergy,iGuess,args=(params,),ftol=1,xtol=params['xtol'])
    print 'Optimized lattice params: ',optimizedLatticeParams
    
    ################################
    print "Storing Results..."
    #-------------------------------

    optAtoms    = ase.io.read('out.traj') #read optimized cell and positions
    with asepar.paropen('energy_forces.pckl') as f: eng,forces=cPickle.loads(str(f.read()))
    resultDict  = mergeDicts([params,trajDetails(optAtoms)
                            ,{'raw_energy':     eng
                            ,'forces_pckl':     cPickle.dumps(forces)
                            ,'latticeopt_pckl': cPickle.dumps(zip(energies,lparams))}])  
    
    with open('result.json', 'w') as f: f.write(json.dumps(resultDict))
    if rank()==0:
        log(params,optAtoms)
        #with open('result.json', 'r') as outfile: json.load(outfile) #WHY DOES THIS CRASH
    return 0


def getInitGuess(structure,cell):
    """
    Convert cell into parameters a,b,c,alpha,beta,gamma
    Depending on structure, return sufficient information required to reconstruct cell (e.g. only 'a' is needed if the structure is known cubic)
    """
    import numpy as np
    
    def angle(v1,v2): return np.arccos(np.dot(v1,np.transpose(v2))/(np.linalg.norm(v1)*np.linalg.norm(v2)))
    a,b,c            = np.linalg.norm(cell[0]),np.linalg.norm(cell[1]),np.linalg.norm(cell[2])
    alpha,beta,gamma = angle(cell[1],cell[2]), angle(cell[0],cell[2]), angle(cell[0],cell[1])

    if   structure in ['fcc','bcc','rocksalt','diamond','cscl','zincblende']: return [a]
    elif structure in ['hexagonal']: return [a,c]
    elif structure in ['triclinic']: return [a,b,c,alpha,beta,gamma]
    else: raise ValueError, 'Bad entry in "structure" field for Atoms object info dictionary: '+structure


def getBulkEnergy(latticeParams,params):
    #For a given set of bravais lattice parameters, optimize atomic coordinates and return minimum energy
    import cPickle,ase,json
    import ase.parallel as asepar 
    import ase.io as aseio
    global energies,lparams

    with asepar.paropen('lastguess.json','w') as f: f.write(json.dumps(list(latticeParams)))
    atomsInitUnscaled = makeAtoms(params)
    atoms = fromParams(atomsInitUnscaled,latticeParams,params['structure'])
    optimizePos(atoms,makeCalc(params),params['fmax'])
    energy,forces = atoms.get_potential_energy(),atoms.get_forces()
    energies.append(energy);lparams.append(latticeParams)
    with asepar.paropen('lattice_opt.log','a') as logfile: logfile.write('%s\t%s\n' %(energy,latticeParams))
    aseio.write('out.traj',atoms)
    with open('energy_forces.pckl','w') as f: f.write(cPickle.dumps((energy,forces)))
    return energy


def fromParams(atomsInput,cellParams,structure): 
    """
    Params is a list of 1 to 6 numbers (a,b,c,alpha,beta,gamma). 
    ANGLES OF INPUT ARE IN RADIANS, ASE CELLS WANT ANGLES IN DEGREES
    Depending on structure, we can construct the cell from a subset of these parameters
    """
    import math     as m
    
    a = cellParams[0]
    if   structure in ['cubic','cscl']:                          cell = [a,a,a,90,90,90]
    elif structure in ['fcc','diamond','zincblende','rocksalt']: cell = [a,a,a,60,60,60]
    elif structure in ['bcc']:                                   cell = [a,a,a,109.47122,109.47122,109.47122]
    elif structure in ['hcp','hexagonal']:                       cell = [a,a,cellParams[1],90,90,120]                                                                                          # Input is assumed to be two parameters, a and c
    elif structure in ['triclinic']:                             cell = [cellParams[0],cellParams[1],cellParams[2],m.degrees(cellParams[3]),m.degrees(cellParams[4]),m.degrees(cellParams[5])] # You should really be using VC relax for this....
    else: raise NotImplementedError, 'fromParams(atomsInput,cellParams,structure) cannot handle unknown structure = '+structure
    
    atoms = atomsInput.copy() 
    atoms.set_cell(cell,scale_atoms=True)

    return atoms


def rank():
    import sys
    # Check for special MPI-enabled Python interpreters:
    if '_gpaw' in sys.builtin_module_names:
        import _gpaw        # http://wiki.fysik.dtu.dk/gpaw
        world = _gpaw.Communicator()
    elif '_asap' in sys.builtin_module_names:
        import _asap # http://wiki.fysik.dtu.dk/asap, can't import asap3.mpi here (import deadlock)
        world = _asap.Communicator()
    elif 'asapparallel3' in sys.modules: # Older version of Asap
        import asapparallel3
        world = asapparallel3.Communicator()
    elif 'Scientific_mpi' in sys.modules:
        from Scientific.MPI import world
    elif 'mpi4py' in sys.modules:
        world = MPI4PY()
    else:
        from ase.parallel import DummyMPI
        world = DummyMPI()# This is a standard Python interpreter:
    rank = world.rank
    size = world.size
    return rank


def mergeDicts(listDicts): 
    import itertools
    return dict(itertools.chain.from_iterable([x.items() for x in listDicts])) #presumes no overlap in keys


def getCluster():
    import os
    hostname = os.environ['HOSTNAME'].lower()
    if      'sh'    in hostname: return 'sherlock'
    elif   'gpu-15' in hostname: return 'sherlock'
    elif    'su'    in hostname: return 'suncat' #important to distinguish suncat2 and 3?
    elif    'kris'  in hostname: return 'kris'
    else: raise ValueError, "getCluster did not detect SH or SU in %s"%hostname


if __name__ == '__main__': OptimizeLatticeScript()

