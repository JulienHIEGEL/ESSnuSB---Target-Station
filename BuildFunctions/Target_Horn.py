# Buildfunctions/Target_Horn.py

from .Element import Element

class Target_Horn(Element): 
    def __init__(self, config):
        super().__init__()
        self.bodies     += f'*\n* ----- Target and Horn bodies ----- \n*\n'
        self.regions    += f'*\n* ----- Target and Horn regions ----- \n*\n'
        self.materials  += f'*\n* ----- Target and Horn materials ----- \n*\n'

        self.Target = config["Target"]
        self.Canister = config["Canister"]
        self.Horn = config["Horn"]
        self.DRAW = config["Horn"]["DRAW"]

    def create_cards(self):    
        ###########################################################
        ######################### BODIES ##########################
        ########################################################### 
        '''
        remarks :
         CaniSup = tgtbox // remove one or another to avoid repetition
        '''
        if not self.DRAW: return
            
        self.bodies += f'RCC thbox    0.0 0.0 0.0  0.0 0.0 {self.Horn["L_box"]} {self.Horn["R_box"]}\n'
        self.bodies += f'RCC tgtbox   0.0 0.0 0.0  0.0 0.0 {self.Canister["L_sup"]} {self.Canister["R_sup"]}\n'
        self.bodies += f'RCC CaniSup  0.0 0.0 0.0  0.0 0.0 {self.Canister["L_sup"]} {self.Canister["R_sup"]}\n'
        self.bodies += f'RCC CaniInf  0.0 0.0 {self.Canister["w_skin"]}  0.0 0.0 {self.Canister["L_inf"]} {self.Canister["R_inf"]}\n'
        self.bodies += f'RCC CaniSkin 0.0 0.0 {self.Canister["w_skin"]}  0.0 0.0 {self.Canister["L_inf"]} {self.Canister["R_skin"]}\n'
        self.bodies += f'RCC target   0.0 0.0 {self.Canister["w_skin"]}  0.0 0.0 {self.Target["L"]} {self.Target["R"]}\n'
        for i in range (len(self.Horn["z"])):
            name = f"h0{i}" if i<10 else f"h{i}"
            self.bodies += f'TRC {name}  0.0 0.0 {self.Horn["z"][i]} 0.0 0.0 {self.Horn["dz"][i]} {self.Horn["R0"][i]} {self.Horn["Rf"][i]}\n'
        
        ###########################################################
        ######################### REGIONS #########################
        ###########################################################
        self.regions += f"""
THBOX        5 +thbox
               -h00 -h01 -h02 -h03 -h04 -h05 -h06 -h07 -h08 -h09-tgtbox
RHALUP       5 +h00-h20 -h30 -h10-target |+h30 -h10-tgtbox
               |+h01-h21 -h31 -h11-target |+h31 -h11-tgtbox
               |+h02-h22 -h32 -h12-target |+h32 -h12-tgtbox
               |+h03-h23 -h33 -h13-target |+h33 -h13-tgtbox
               |+h04-h24 -h34 -h14-target |+h34 -h14-tgtbox
               |+h05-h25 -h35 -h15-target |+h35 -h15-tgtbox
               |+h06-h26 -h36 -h16-target |+h36 -h16-tgtbox
               |+h07-h27 -h37 -h17-target |+h37 -h17-tgtbox
               |+h08-h28 -h38 -h18-target |+h38 -h18-tgtbox
               |+h09-h29 -h39 -h19-target |+h39 -h19-tgtbox
RHBF         5 +h20-h30-tgtbox
               |+h21-h31-tgtbox
               |+h22-h32-tgtbox
               |+h23-h33-tgtbox
               |+h24-h34-tgtbox
               |+h25-h35-tgtbox
               |+h26-h36-tgtbox
               |+h27-h37-tgtbox
               |+h28-h38-tgtbox
               |+h29-h39-tgtbox
RHIN         5 +h10-tgtbox   |+h11-tgtbox   |+h12-tgtbox   |+h13-tgtbox  |+h14-tgtbox
               |+h15-tgtbox  |+h16 -tgtbox |+h17 -tgtbox |+h18 -tgtbox  |+h19-tgtbox
CANIBOX      5 +CaniSup -CaniInf | +CaniSkin -target
CANICOOL     5 +CaniInf -CaniSkin
TARGET       5 +target
"""
        
        ###########################################################
        ###################### MAT & ASSIGNMA #####################
        ###########################################################
        
        self.materials += f"""MATERIAL         22.                  3.                              TITAN60
ASSIGNMA     TITAN60    TARGET
ASSIGNMA    TITANIUM   CANIBOX
ASSIGNMA      HELIUM  CANICOOL
ASSIGNMA      HELIUM     THBOX
ASSIGNMA    ALUMINUM    RHALUP
ASSIGNMA      HELIUM      RHBF                            1.
ASSIGNMA      HELIUM      RHIN
"""

    def add_regions(self, cards):
        lines = cards.splitlines()

        for i, card in enumerate(lines):
            if card.startswith("EXPAREA      5 +exparea"):
                card += " -thbox"
                lines[i] = card
        return "\n".join(lines) + "\n"   
        