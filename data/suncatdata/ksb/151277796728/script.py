

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
    atoms   = make_atoms(prms)

    aseio.write('init.traj',atoms)         # Step 3

    return prms,atoms


def rank():
    """
    Identify which parallel processor is executing this code
    """
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
    elif 'mpi4py' in sys.modules: world = MPI4PY()
    else:
        from ase.parallel import DummyMPI
        world = DummyMPI()# This is a standard Python interpreter:
    rank = world.rank #size = world.size
    return rank


def get_cluster():
    import os
    hostname = os.environ['HOSTNAME'].lower()
    if  'sh'in hostname or  'gpu-' in hostname:
        sher = os.environ['SHERLOCK']
        if   sher == '1':  return 'sherlock'
        elif sher == '2': return 'sherlock2'
        else: raise ValueError, 'What cluster is this? '+sher

    elif    'su'      in hostname: return 'suncat'
    elif    'nid'     in hostname: return 'nersc'
    else: raise ValueError, "clusterRoot could not parse HOSTNAME: %s"%hostname


def make_atoms(params):
    import cPickle
    return cPickle.loads(str(params['inittraj_pckl']))


def log(params):
    import datalog,json
    if params.get('kwargs') is None: datalog.log()
    else:                            datalog.log(**(json.loads(params['kwargs'])))


def vasp_script():
    import ase.calculators.vasp as vasp_calculator
    import json,multiprocessing,time,os,subprocess

    params,atoms = initialize()      # Remove old .out/.err files, load from fw_spec, and write 'init.traj'
    #############################################
    print "Initializing VASP calculator..."
    #-----------------------------------------

    calc  =  vasp_calculator.Vasp(encut   = params['pw']
                                ,setups   = json.loads(params['psp']) # dictionary {'Cu':'_s'}
                                ,xc       = params['xc']
                                ,gga      = params['gga']      # 'BF'
                                ,luse_vdw = params['luse_vdw'] # True
                                ,zab_vdw  = params.get('zab_vdw')
                                ,kpts     = json.loads(params['kpts'])
                                ,npar     = 1                  # use this if you run on one node (most calculations).  see suncat confluence page for optimal setting
                                ,kpar     = 1
                                ,gamma    = params['gamma']    # True,  # Gamma-centered (defaults to Monkhorst-Pack)
                                ,ismear   = 0                  # assume gaussian smearing, we can make this parameter if necessary
                                ,algo     = params['algo']     #'fast',
                                ,nelm     = params['maxstep']
                                ,sigma    = params['sigma']
                                ,ibrion   = params['ibrion']   #2
                                ,nelmdl   = params['nelmdl']
                                ,isif     = 3 if params['jobkind']=='vcrelax' else 2
                                ,ediffg   = -params['fmax']                        # forces
                                ,ediff    = params['econv']                        # energy conv. both of these are for the internal relaxation, ie nsw
                                ,prec     = params['prec']                         #'Accurate'
                                ,nsw      = params['ionic_steps']                  # use ASE ?
                                ,ispin    = 2 if params['spinpol'] else 1
                                ,lreal    = params['lreal']                        # automatically decide to do real vs recip space calc
                                ,ldipol   = params['dipole']                       #True,
                                ,lvhar    = params['lvhar']                        #True,
                                ,dipol    = params.get('dipol')                    #(0.5,0.5,0.5),
                                ,idipol   = 3 if params['dipole'] else None
                                ,icharg   = 1)                                     # start from CHGCAR if icharg = 1

    atoms.set_calculator(calc)

    ############################################
    print "Generating VASP Input files with ASE"
    #------------------------------------------
    try:
        proc = multiprocessing.Process(target=atoms.get_potential_energy)
        proc.start();time.sleep(10)
        if proc.is_alive(): proc.terminate()
    except IOError: pass #on nersc, we get io error

    #######################
    print "Running VASP natively..."
    #----------------------

    loader = {'sherlock':  "ml vasp/5.4.1.intel.gpu"
             ,'sherlock2': "ml chemistry vasp/5.4.1"
             ,'nersc':      "module load vasp-tpc/5.4.1"}

    vaspout, vasperr = subprocess.Popen(loader[get_cluster()]+';which vasp_std;srun -n 32 vasp_std', stdout=subprocess.PIPE, stderr=subprocess.PIPE,shell=True).communicate()
    print 'VASP output:\n',vaspout
    with open('OUTCAR','r') as f:
        if 'General timing and accounting informations for this job' in f.read():
            if rank()==0: log(params)
            return 0
    raise ValueError, vasperr


if __name__ == '__main__': vasp_script()

