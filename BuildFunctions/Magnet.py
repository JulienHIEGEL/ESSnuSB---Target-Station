# BuilFunctions/Magnet.py

from .Element import Element

class Magnet(Element): 
    def __init__(self, config):
        super().__init__()
        self.bodies     += f'*\n* ----- Magnet bodies ----- \n*\n'
        self.regions    += f'*\n* ----- Magnet regions ----- \n*\n'
        self.materials  += f'*\n* ----- Magnet materials ----- \n*\n'

        self.Dipole = config["Dipole"]
        self.DRAW = config["Dipole"]["DRAW"]

    def create_cards(self):
        if not self.DRAW: return
        ###########################################################
        ######################### BODIES ##########################
        ########################################################### 

        x0, xf = self.Dipole["x"]
        y0, yf = self.Dipole["y"]
        z0, zf = self.Dipole["z"]
    
        xcoils = self.Dipole["xcoils"]
        ycoils = self.Dipole["ycoils"]
    
        w_ycoils = self.Dipole["w_ycoils"]
        y_airgap = [ycoils[0] + w_ycoils, ycoils[1] - w_ycoils]
    
        w_coils = self.Dipole["w_coils"]
        
        self.bodies += f"""RPP magnet    {x0} {xf} {y0} {yf} {z0} {zf}
RPP coils    {xcoils[0]} {xcoils[1]} {ycoils[0]} {ycoils[1]} {z0} {zf}
RPP airgap   {xcoils[0]} {xcoils[1]} {y_airgap[0]} {y_airgap[1]} {z0} {zf}
RPP Fecoils  {xcoils[0] + w_coils} {xcoils[1] - w_coils} {ycoils[0]} {ycoils[1]} {z0 + w_coils} {zf - w_coils}
"""
        ###########################################################
        ######################### REGIONS #########################
        ###########################################################
        self.regions += f"""MAGNET       5 +magnet -airgap -(coils-Fecoils) 
COIL         5 +coils -Fecoils -airgap
BFIELD       5 +airgap
"""
        ###########################################################
        ###################### MAT & ASSIGNMA #####################
        ###########################################################
        
        self.materials += f"""ASSIGNMA        IRON    MAGNET
ASSIGNMA      HELIUM    BFIELD                            1.
ASSIGNMA      COPPER      COIL
"""
        
    def add_regions(self, cards):
        lines = cards.splitlines()

        for i, card in enumerate(lines):
            if card.startswith("DT           5 +dt -exparea"):
                card += " -magnet"
                lines[i] = card
        return "\n".join(lines) + "\n"   

