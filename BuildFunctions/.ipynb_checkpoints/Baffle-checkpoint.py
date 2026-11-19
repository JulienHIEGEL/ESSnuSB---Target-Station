# Buildfunctions/Target_Horn.py

from .Element import Element

class Baffle(Element): 
    def __init__(self, config):
        super().__init__()
        self.bodies     += f'*\n* ----- Baffle bodies ----- \n*\n'
        self.regions    += f'*\n* ----- Baffle regions ----- \n*\n'
        self.materials  += f'*\n* ----- Baffle materials ----- \n*\n'

        self.Baffle = config["Baffle"]
        self.DRAW = self.Baffle["DRAW"]

    def create_cards(self):    
        ###########################################################
        ######################### BODIES ##########################
        ########################################################### 
        '''
        remarks :
         CaniSup = tgtbox // remove one or another to avoid repetition
        '''
        if not self.DRAW: return
        self.bodies += f'TRC baffle  0.0 0.0 {self.Baffle["z_0"]} 0.0 0.0 {self.Baffle["length"]} {self.Baffle["outer_R"]} {self.Baffle["outer_R"]}\n'
        self.bodies += f'TRC baffleH  0.0 0.0 {self.Baffle["z_0"]} 0.0 0.0 {self.Baffle["length"]} {self.Baffle["inner_R1"]} {self.Baffle["inner_R2"]}\n'

        ###########################################################
        ######################### REGIONS #########################
        ###########################################################
        self.regions += f"""
BAFFLE        5 +baffle -baffleH
BAFFLEH       5 +baffleH
"""
        
        ###########################################################
        ###################### MAT & ASSIGNMA #####################
        ###########################################################
        
        self.materials += f"""
ASSIGNMA      CARBON    BAFFLE
ASSIGNMA      HELIUM   BAFFLEH
"""

    def add_regions(self, cards):
        lines = cards.splitlines()

        for i, card in enumerate(lines):
            if card.startswith("EXPAREA      5 +exparea"):
                card += " -baffle"
                lines[i] = card
        return "\n".join(lines) + "\n"   
        