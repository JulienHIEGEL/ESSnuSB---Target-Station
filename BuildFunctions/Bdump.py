# BuilFunctions/Bdump.py

from .Element import Element
import numpy as np

class Bdump(Element): 
    def __init__(self, config):
        super().__init__()
        self.bodies     += f'*\n* ----- Beam Dump bodies ----- \n*\n'
        self.regions    += f'*\n* ----- Beam Dump regions ----- \n*\n'
        self.materials  += f'*\n* ----- Beam Dump materials ----- \n*\n'

        self.bd = config["BeamDump"]
        self.DRAW = config["BeamDump"]["DRAW"]

    def create_cards(self):
        if not self.DRAW: return
        ###########################################################
        ######################### BODIES ##########################
        ########################################################### 

        x = [self.bd["x"][0], self.bd["x"][1]]
        y = [self.bd["y"][0], self.bd["y"][1]]
        z = [self.bd["z0"], self.bd["z0"] + self.bd["width"]]
                    
        self.bodies += f"""RPP bdump    {x[0]} {x[1]} {y[0]} {y[1]} {z[0]} {z[1]}\n"""
        
        ###########################################################
        ######################### REGIONS #########################
        ###########################################################
        self.regions += f"""BDUMP       5 +bdump \n"""
        ###########################################################
        ###################### MAT & ASSIGNMA #####################
        ###########################################################
        
        self.materials += f"""ASSIGNMA      CARBON    BDUMP\n"""

    def add_regions(self, cards):
        lines = cards.splitlines()

        for i, card in enumerate(lines):
            if card.startswith("DT           5 +dt -exparea"):
                card += " -bdump"
                lines[i] = card
        return "\n".join(lines) + "\n"   