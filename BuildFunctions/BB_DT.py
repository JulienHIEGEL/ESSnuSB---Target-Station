# BuilFunctions/BB_DT.py

from .Element import Element

class BB_DT(Element): 
    def __init__(self, config):
        super().__init__()
        self.bodies     += f'*\n* ----- BlackBody_DecayTunnel bodies ----- \n*\n'
        self.regions    += f'*\n* ----- BlackBody_DecayTunnel regions ----- \n*\n'
        self.materials  += f'*\n* ----- BlackBody_DecayTunnel materials ----- \n*\n'
        self.DRAW = True
        self.BB = config["BlkBody"]
        self.DT = config["DT"]
        self.park = config["Parking"]
        
    def create_cards(self):
        ###########################################################
        ######################### BODIES ##########################
        ########################################################### 

        xDT0, xDTf = self.DT["x"]
        yDT0, yDTf = self.DT["y"]
        zDT0, zDTf = self.DT["z"]

        w_wall = self.DT["w_wall"]
        w_vessel = self.DT["w_vessel"]

        self.bodies += f'SPH blkprk   {self.park["x"]} {self.park["y"]} {self.park["z"]} {self.park["Rout"]}\n'
        self.bodies += f'SPH vacprk   {self.park["x"]} {self.park["y"]} {self.park["z"]} {self.park["Rin"]}\n'
        
        self.bodies += f'SPH blkbody  {self.BB["x"]} {self.BB["y"]} {self.BB["z"]} {self.BB["R"]}\n'
        self.bodies += f'RPP dt       {xDT0} {xDTf} {yDT0} {yDTf} {zDT0} {zDTf}\n'
        self.bodies += f'RPP dt_wall  {xDT0 - w_wall} {xDTf + w_wall} {yDT0 - w_wall} {yDTf + w_wall} {zDT0 - w_wall} {zDTf + w_wall}\n'
        self.bodies += f'RPP vessel   {xDT0 - w_vessel} {xDTf + w_vessel} {yDT0 - w_vessel} {yDTf + w_vessel} {zDT0 - w_vessel} {zDTf + w_vessel}\n'
        self.bodies += f'RPP exparea  {xDT0} {xDTf} {yDT0} {yDTf} {zDT0} {self.DT["zexparea"]}\n'

        ###########################################################
        ######################### REGIONS #########################
        ###########################################################
        self.regions += f"""BLKBODY      5 +blkbody -dt_wall | +blkprk -vacprk
PARKING      5 +vacprk -blkbody
DTLAYOUT     5 +dt_wall -dt -vessel
EXPAREA      5 +exparea
DT           5 +dt -exparea
VESSEL       5 +vessel -dt
"""
        ###########################################################
        ###################### MAT & ASSIGNMA #####################
        ###########################################################
        
        self.materials = f"""ASSIGNMA    BLCKHOLE   BLKBODY
ASSIGNMA      VACUUM   PARKING
ASSIGNMA    PORTLAND  DTLAYOUT
ASSIGNMA      HELIUM   EXPAREA
ASSIGNMA      HELIUM        DT
ASSIGNMA        IRON    VESSEL
"""