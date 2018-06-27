import json

def mk_relaxed_surface(rawstrs: str
                      ,adsstrs: str
                      ,orderstr : str
                      ,stordirs : str
                      ,facetstrs : str
                      ) -> RelaxedSurface:
    raw1,raw2 = list(map(json.loads,rawstrs.split('@@@'))
    ads1,ads2 = adsstrs.split('@@@')
    orderstrs = orderstr.split('@@@')
    sd1,sd2   = stordirs.split('@@@')
    fa1,fa2   = list(map(json.loads,facetstrs.split('@@@'))

    class RelaxedSurface(object):
        """columns needed: ."""
        def __init__(self,raw1,raw2,ads1,ads2,sd1,sd2,fa1,fa2): # type: ignore

            self.initatoms  = json_to_traj(raw1) # ase.Atoms
            self.finalatoms = json_to_traj(raw2) # ase.Atoms
            self.adsorbates = json.loads(adsorbates)                          # [[Int]]
            self.facet      = json.loads(facet)                               # (Int,Int,Int)
            self.cell       = json.loads(cell)
            self.bare       = self.initatoms.copy()                           # ase.Atoms
            #self.stordir    = storage_directory                               # for debugging purposes
            for i in flatten(self.adsorbates): del self.bare[i]


        def coverage(self):  return len(self.adsorbates)/float(len(self.sites)) # type: ignore

        def get_adsorbates(self):  # type: ignore
            """ Get a list of ase.Atoms objects, corresponding to gas phase
                representations of the adsorbates, if any"""
            output = []
            for ads in self.adsorbates:
                ads_atoms = self.finalatoms.copy()
                for i in ads: del ads_atoms[i]
                output.append(ads_atoms.copy())
            return output

        def _get_sites(self,site_type='all',sym_reduce=0): # type: ignore
            """Use pymatgen site finder to locate surface sets"""
            return [ads_site.Site(self.bare,s,self.facet) for s in get_sites(self.bare,self.facet,site_type,sym_reduce)]

        def get_xy(self): # type: ignore
            """Approximately get x,y dimensions (in # of atoms) of a slab """
            [lx,ly,_] = map(np.linalg.norm,self.cell)
            xyproduct = len(self._get_sites(site_type='ontop')) # x * y = a
            ratio     = lx / ly                                 # x / y = b

            def rounder(z): return int(max(1,round(z)))  # type: ignore
            x = rounder((xyproduct * ratio)**0.5)  # type: ignore
            y = rounder(x / ratio)                  # type: ignore
            if x*y == xyproduct: return (x,y)
            else:
                if 2 * x * y == xyproduct: return (2*x,2*y)
                else:
                    print('something wrong in get_xy: xyproduct=%d,ratio=%f,x=%d,y=%d \n%s'%(xyproduct,ratio,x,y,self.stordir))
                    return (None,None)

        def get_layers(self): # type: ignore
            """Approximately get the number of layers of a slab"""
            zs    = [p[2] for p in self.bare.get_positions()]
            zlist = [zs[0]]
            for z in zs:
                if all([abs(zz -z) > 1.5 for zz in zlist]): zlist.append(z)
            return len(zlist)

        def assign_sites(self): # type: ignore
            """Maps list of adsorbates to list of Site objects"""
            ads_sites = []
            for ads in self.get_adsorbates():
                com = ads.get_center_of_mass()
                _,min_site = min([(np.linalg.norm(com-s.pos),s) for s in self.get_sites()])
                ads_sites.append(min_site)
            return ads_sites

        def reconstructed(self): # type: ignore
            """True if surface reconstructed"""
            raise NotImplementedError

        def surface_area(self): # type: ignore
            return np.linalg.norm(np.cross(self.cell[0],self.cell[1]))

        def constraints_in_middle(self): # type: ignore
            """
            is it a 'symmetric' slab or not?
            just testing if constrained atoms are not at bottom
            use case: we don't care about 'surface energy' unless slab is symmetric
            """
            atoms = self.finalatoms.copy()
            atoms.center()
            minConstrained,minUnconstrained = 100000,100000
            for a in atoms:
                if a.index in atoms.constraints[0].get_indices():
                    if a.z < minConstrained: minConstrained = a.z
                elif a.z < minUnconstrained: minUnconstrained=a.z
            return minUnconstrained < minConstrained

        def get_vacuum(self): # type: ignore
            """Length of vacuum layer between periodic images (in Z direction)"""
            atoms = self.bare.copy()
            atoms.center()   # In case dealing with Nerds Rope & Co.
            c     = np.linalg.norm(atoms.get_cell()[2])
            zs    = [a.position[2] for a in atoms]
            return c - (max(zs) - min(zs))

    return RelaxedSurface(raw1,raw2,ads1,ads2,sd1,sd2,fa1,fa2)
