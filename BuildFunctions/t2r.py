# BuilFunctions/t2r.py

from .Element import Element
import numpy as np

class T2R(Element): 
    def __init__(self, config):
        super().__init__()
        self.bodies     += f'*\n* ----- t2r bodies ----- \n*\n'
        self.regions    += f'*\n* ----- t2r regions ----- \n*\n'
        self.materials  += f'*\n* ----- t2r materials ----- \n*\n'
        self.transforms += f'\n*\n* ----- t2r transforms ----- \n*\n'

        self.t2r= config["t2r"]
        self.DRAW = config["t2r"]["DRAW"]


    def create_cards(self):
        if not self.DRAW: return
        ###########################################################
        ######################### BODIES ##########################
        ########################################################### 
    
        width  = self.t2r["width"]
        L = self.t2r["length"]
        theta = self.t2r["theta"]
        w_walls = self.t2r["w_walls"] 
        w_vessel = self.t2r["w_vessel"] 
    
        xwall = [-0.5*width - w_walls, 0.5*width + w_walls]
        ywall = [-0.5*width - w_walls, 0.5*width + w_walls]
        zwall = [0 - w_walls, L + w_walls]
    
        xvessel = [-0.5*width + w_vessel, 0.5*width - w_vessel]
        yvessel = [-0.5*width + w_vessel, 0.5*width - w_vessel]
        zvessel = [0 + w_vessel, L - w_vessel]
    
        Trx, Try = self.compute_transformation(width, theta, self.t2r["z_start"], self.t2r["x_start"])
        
        self.transforms += f"""ROT-DEFI         200         0{np.abs(theta):>10}{Try:>10.4f}{0:>10}{Trx:>10.4f}ttline"""
        
        self.bodies += f"""$start_transform ttline 
RPP ttline   {-0.5*width} {0.5*width} {-0.5*width} {0.5*width} {0} {L}
RPP ttwall   {xwall[0]} {xwall[1]} {ywall[0]} {ywall[1]} {zwall[0]} {zwall[1]}
RPP ttvessel {xvessel[0]} {xvessel[1]} {yvessel[0]} {yvessel[1]} {zvessel[0]} {zvessel[1]}
$end_transform 
"""
        
        ###########################################################
        ######################### REGIONS #########################
        ###########################################################
        self.regions += f"""TTLINE       5 +ttvessel -dt\n"""
        ###########################################################
        ###################### MAT & ASSIGNMA #####################
        ###########################################################
        
        self.materials += f"""ASSIGNMA      HELIUM   TTLINE\n"""

    def compute_transformation(self, width, theta, Xf, Yf):
        '''
        compute the translation parameter (for the ttline) to be applied before rotation of theta degrees. 

        Xf and Yf are HERE the left bottom point of the Beam Dump 
        -> the computation SHOULD be outside
        
        width : width of the t2r 
                should be given in [cm]
        theta : for ESSnuSB+, it should be negative (-29.5)
                should be given in [degree]
        Xf    : in the zx-plane, Xf refers to the ending point (after transformation) 
                on the z-axis of the left bottom corner of the t2r
                should be given in [cm]
        Yf    : in the zx-plane, Yf refers to the ending point (after transformation) 
                on the x-axis of the left bottom corner of the t2r
                should be given in [cm]        
        '''
        width = width*1e-2
        theta = np.deg2rad(theta)
        #Zdif = width / np.sin(theta) 
        
        Xf = Xf*1e-2 #+ Zdif
        Yf = Yf*1e-2

        # debug
        self.Xf = Xf 
        self.Yf = Yf

        with open("BuildFunctions/Quad.py", "r") as f:
            lines = f.readlines()
        if lines[52].strip().startswith("Xf_t2r = "):
            lines[52] = f"        Xf_t2r = {Xf*100} # [cm] Reference because we know this point\n"
        if lines[53].strip().startswith("Yf_t2r = "):
            lines[53] = f"        Yf_t2r = {Yf*100} # [cm] Reference because we know this point\n"
        with open("BuildFunctions/Quad.py", "w") as f:
            f.writelines(lines)

        
        # initial coordinate of the left bottom corner point
        X0 = 0e-2
        Y0 = - width/2
        
        
        # Inversion of the rotation + translation
        '''
        R = rotation, R_1 = inverse rotation, T = translation
        X = initial coordinate, Y = final coordinate
        
        R = (+cos -sin)    R_1 = (+cos +sin)
            (+sin +cos)          (-sin +cos)
    
        Full transformation (ROTDEFI)
        Y = R(X+T)
        
        T = R_1(Y) - X
        --> T =
             (+xf cos + yf sin - x0)
             (-xf sin + yf cos - y0)
        xf, yf (final)
        x0, y0 (initial)
        '''
        R_1Yx = Xf * np.cos(theta) + Yf * np.sin(theta)
        R_1Yy = -Xf * np.sin(theta) + Yf * np.cos(theta)
        
        Trx = R_1Yx - X0
        Try = R_1Yy - Y0
    
        return Trx*100, Try*100

    def add_regions(self, cards):
        lines = cards.splitlines()

        for i, card in enumerate(lines):
            if card.startswith("BLKBODY      5 +blkbody -dt_wall"):
                card = "BLKBODY      5 +blkbody -dt_wall -ttwall | +blkprk -vacprk"
                lines[i] = card
            if card.startswith("DTLAYOUT     5 +dt_wall -dt -vessel"):
                card = "DTLAYOUT     5 +dt_wall -dt -vessel -ttline |+ttwall -ttline -vessel"
                lines[i] = card
            if card.startswith("VESSEL       5 +vessel -dt"):
                card = "VESSEL       5 +vessel -dt -ttline |+ttline -ttvessel -dt"
                lines[i] = card                
        return "\n".join(lines) + "\n"   