# BuilFunctions/Collimator.py

from .Element import Element

class Collimator(Element): 
    def __init__(self, config):
        super().__init__()
        self.bodies     += f'*\n* ----- Collimator bodies ----- \n*\n'
        self.regions    += f'*\n* ----- Collimator regions ----- \n*\n'
        self.materials  += f'*\n* ----- Collimator materials ----- \n*\n'

        self.Collimator = config["Collimator"]
        self.DRAW = config["Collimator"]["DRAW"]

    def create_cards(self):   
        if not self.DRAW: return
            
        ###########################################################
        ######################### BODIES ##########################
        ########################################################### 

        x0, xf = self.Collimator["x"]
        y0, yf = self.Collimator["y"]
        z0, zf = self.Collimator["z"]
        Lclmt = self.Collimator["z"][1] - self.Collimator["z"][0]
        
        self.bodies += f'RPP clmt     {x0} {xf} {y0} {yf} {z0} {zf}\n'
        self.bodies += f'RCC hclmt    0.0 0.0 {z0} 0.0 0.0 {Lclmt} {self.Collimator["R"]}\n'
        self.bodies += f'RCC clmtW    0.0 0.0 {z0} 0.0 0.0 {Lclmt} {self.Collimator["R_w"]}\n'

        ###########################################################
        ######################### REGIONS #########################
        ###########################################################
        self.regions += f"""CLMT         5 +clmt -hclmt -clmtW
CLMTHOLE     5 +hclmt
CLMTW        5 +clmtW -hclmt
"""
        ###########################################################
        ###################### MAT & ASSIGNMA #####################
        ###########################################################
        
        self.materials = f"""ASSIGNMA        IRON      CLMT
ASSIGNMA      HELIUM  CLMTHOLE
ASSIGNMA    TUNGSTEN     CLMTW
"""

    def add_regions(self, cards):
        lines = cards.splitlines()

        for i, card in enumerate(lines):
            if card.startswith("DT           5 +dt -exparea"):
                card += " -clmt"
                lines[i] = card
        return "\n".join(lines) + "\n"        