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

        yairgap = self.Dipole["airgap"]
        
        xcoils = self.Dipole["xcoils"]
    
        w_ycoils = self.Dipole["w_ycoils"]
        w_coils = self.Dipole["w_coils"]
        w_shield= self.Dipole["w_shield"]
        
        ycoils = [- (yairgap + w_shield + w_ycoils),  (yairgap + w_shield + w_ycoils)]
        yWplates=[- (yairgap + w_shield),  (yairgap + w_shield)]
        
        self.bodies += f"""RPP magnet    {x0} {xf} {y0} {yf} {z0} {zf}
RPP coils    {xcoils[0]} {xcoils[1]} {ycoils[0]} {ycoils[1]} {z0} {zf}
RPP airgap   {xcoils[0]} {xcoils[1]} {-yairgap} {yairgap} {z0} {zf}
RPP Fecoils  {xcoils[0] + w_coils} {xcoils[1] - w_coils} {ycoils[0]} {ycoils[1]} {z0 + w_coils} {zf - w_coils}
RPP Wplates  {xcoils[0]} {xcoils[1]} {yWplates[0]} {yWplates[1]} {z0} {zf}
RPP DSshield {-250} {250} {-250} {250} {zf} {zf + 50}
RPP DSairgap {-250} {xcoils[1]} {-yairgap} {yairgap} {zf} {zf + 50}
"""
        ###########################################################
        ######################### REGIONS #########################
        ###########################################################
        self.regions += f"""MAGNET       5 +magnet -(coils-Fecoils) -(Wplates-Fecoils) -airgap
COIL         5 +coils -Fecoils -Wplates
BFIELD       5 +airgap
WPLATES      5 +Wplates -Fecoils -airgap
DSSHIELD     5 +DSshield -DSairgap
DSAIRGAP     5 +DSairgap
"""
        ###########################################################
        ###################### MAT & ASSIGNMA #####################
        ###########################################################
        
        self.materials += f"""ASSIGNMA        IRON    MAGNET
ASSIGNMA      HELIUM    BFIELD                            1.
ASSIGNMA      COPPER      COIL
ASSIGNMA    TUNGSTEN   WPLATES
ASSIGNMA        IRON  DSSHIELD
ASSIGNMA      HELIUM  DSAIRGAP
"""
        
    def add_regions(self, cards):
        lines = cards.splitlines()

        for i, card in enumerate(lines):
            if card.startswith("DT           5 +dt -exparea"):
                card += " -magnet -DSshield"
                lines[i] = card
        return "\n".join(lines) + "\n"   

