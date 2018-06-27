

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


def makeCalc(p):
    import json

    def makeGPAWcalc():
        from gpaw import GPAW,PW,Davidson,Mixer,MixerSum,FermiDirac,setup_paths

        if p['psp'] == 'oldpaw':
            setup_paths.insert(0, '/scratch/users/ksb/gpaw/oldpaw/gpaw-setups-0.6.6300/')
            psp = 'paw'
        else: psp = p['psp']

        return GPAW(mode         = PW(p['pw'])
                    ,xc          = p['xc']
                    ,kpts        = json.loads(p['kpts'])
                    ,spinpol     = p['spinpol']
                    ,convergence = {'energy':p['econv']} #eV/electron
                    ,mixer       = ((MixerSum(beta=p['mixing'],nmaxold=p['nmix'],weight=100))
                                    if  p['spinpol'] else (Mixer(beta=p['mixing'],nmaxold=p['nmix'],weight=100)))
                    ,maxiter       = p['maxstep']
                    ,nbands        = p['nbands']
                    ,occupations   = FermiDirac(p['sigma'])
                    ,setups        = psp
                    ,eigensolver   = Davidson(5)
                    ,poissonsolver = None
                    ,txt           ='log'
                    ,symmetry={'do_not_symmetrize_the_density': True})

    def makeQEcalc():
        import json
        from espresso import espresso

        pspDict =   {'sherlock': {'gbrv15pbe':'/home/vossj/suncat/psp/gbrv1.5pbe'}
                    ,'suncat':   {'gbrv15pbe':'/nfs/slac/g/suncatfs/sw/external/esp-psp/gbrv1.5pbe'}}
        pspPath =  pspDict[getCluster()][p['psp']]

        return espresso( pw         = p['pw']
                        ,dw         = p['dw']
                        ,xc         = p['xc']
                        ,kpts       = json.loads(p['kpts'])
                        ,spinpol    = p['spinpol']
                        ,convergence=   {'energy':  p['econv']
                                        ,'mixing':  p['mixing']
                                        ,'nmix':    p['nmix']
                                        ,'maxsteps':p['maxstep']
                                        ,'diag':    'david'}
                        ,nbands     = p['nbands']
                        ,sigma      = p['sigma']
                        ,dipole     = {'status': p['dipole']}
                        ,outdir     = 'calcdir'
                        ,startingwfc= 'atomic+random'
                        ,psppath    = pspPath
                        ,mode       = 'scf'
                        ,output     = {'removesave':True})

    if p['dftcode']=='gpaw': return makeGPAWcalc()
    else:                    return makeQEcalc()


def optimizePos(atoms,calc,fmax):
    import ase.optimize as aseopt

    atoms.set_calculator(calc)
    dyn = aseopt.BFGS(atoms=atoms, logfile='qn.log', trajectory='qn.traj',restart='qn.pckl')
    dyn.run(fmax=fmax)


def log(params):
    import datalog,json
    if params.get('kwargs') is None: datalog.log()
    else:                            datalog.log(**(json.loads(params['kwargs'])))


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


def RelaxScript():
    import ase,json,cPickle,os
    import ase.parallel   as asepar

    #######################
    print "Initializing..."
    #----------------------

    params,initatoms = initialize()  # Remove old .out/.err files, load from fw_spec, and write 'init.traj'

    if rank()==0:
        if os.path.exists('qn.traj'):
            if os.stat('qn.traj').st_size > 100:
                initatoms = ase.io.read('qn.traj')
                print '\treading from qn.traj...'
            else:
                os.remove('qn.traj')
                print 'removed empty qn.traj'
        if os.path.exists('qn.log') and os.stat('qn.log').st_size < 100:
            os.remove('qn.log')
            print '\tremoved empty qn.log...'

    #######################
    print "Optimizing positions..."
    #----------------------
    optimizePos(initatoms,makeCalc(params),params['fmax'])

    ############################
    print "Storing Results..."
    #--------------------------
    ase.io.write('final.traj',initatoms)
    e0 = initatoms.get_potential_energy()
    f0 = initatoms.get_forces()
    optAtoms = ase.io.read('final.traj')
    resultDict  = mergeDicts([params,trajDetails(optAtoms)
                                ,{'raw_energy': e0
                                ,'forces_pckl':cPickle.dumps(f0)} ])

    with open('result.json', 'w') as f: f.write(json.dumps(resultDict))
    if rank()==0: log(params)
    return 0


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
    if      'sh'    in hostname or  'gpu-' in hostname:    return 'sherlock'
    elif    'su'    in hostname:                           return 'suncat'
    else: raise ValueError, "clusterRoot did not detect SH GPU- or SU in %s"%hostname


if __name__ == '__main__': RelaxScript()

