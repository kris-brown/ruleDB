import typing as typ
import json
def load_comp(i             : int
             ,raw_structure : str
             ,raw_ads       : str
             ) -> typ.List[typ.Tuple[int,int,int
                                    ,int,int,int
                                    ,float,float,float
                                    ,int,int,int]]:
    """
    Performs basic composition analysis, creating a row for every STRUCTURE x ELEMENT
    pair for which the element exists in the structure.

    Intended insert columns are:

    ['structure_id','element_id',	'has'
               ,'count',	   'count_ads', 	'count_fixed'
               ,'frac', 	   'frac_ads', 		'frac_fixed'
               ,'count_norm',  'count_ads_norm','count_fixed_norm']
    """

    # Load raw strings
    atomdata = json.loads(raw_structure)['atomdata']
    ads      = [y for z in json.loads(raw_ads) for y in z]

    # Set up dictionaries of stoichiometries
    symbs      = [x['number'] for x in atomdata]
    constsymbs = [x['number'] for x in atomdata if x['constrained']]
    adssymbs   = [symbs[z] for z in ads]

    setsymbs  = set(symbs)

    symbdict  = {s:symbs.count(s)      for s in setsymbs}
    aSymbdict = {s:adssymbs.count(s)   for s in setsymbs}
    fSymbdict = {s:constsymbs.count(s) for s in setsymbs}

    sum_ = sum(symbdict.values())
    sumA = sum(aSymbdict.values())
    sumF = sum(fSymbdict.values())

    output   = []

    for j in setsymbs:
        count       = symbdict[j]
        count_ads   = aSymbdict[j]
        count_fixed = fSymbdict[j]

        frac        = round(count / sum_,3)
        frac_ads    = 0 if sumA == 0 else round(count_ads / sumA,3)
        frac_fixed  = 0 if sumF == 0 else round(count_fixed / sumF,3)

        # NEED A WAY TO GET GCD OF A LIST OF INTEGERS

    return [(i,     j,        1
            ,count,count_ads,count_fixed
            ,frac, frac_ads, frac_fixed
            ,0,     0,          0)]
