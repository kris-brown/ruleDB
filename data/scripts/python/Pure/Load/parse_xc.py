import typing as typ

from dbgenPython.core.parsing import parse_line

def parse_xc(files   : typ.Dict[str,str]
            ,dftcode : str
            ) -> str:
    """
    Finds the exchange correlation functional
    """
    log   = files.get('log')
    pwinp = files.get('pwinp')
    incar = files.get('incar')

    if dftcode == 'gpaw':
        xc = parse_line(log,'xc:',0).split(':')[-1].strip()

    elif dftcode == 'quantumespresso':
        xc = parse_line(pwinp,'input_dft',0).split('=')[-1].replace(',','').replace("'",'').strip()

    elif dftcode == 'vasp':
        if parse_line(incar,'GGA',0).split('=')[-1].strip() == 'PE':
            if True: #if not BEEF?
                xc = 'PBE'
        else:
            raise NotImplementedError('New functional in vasp?')
    else:
        raise NotImplementedError('New DFT code? '+dftcode)


    if xc.lower() == 'pbe':
        return 'PBE'
    elif xc.lower() in ['beef','beef-vdw']:
        return 'BEEF'
    elif xc.lower() == 'rpbe':
        return 'RPBE'
    elif xc.lower() == 'mbeef':
        return 'mBEEF'
    else:
        raise ValueError("weird xc: ",xc)
