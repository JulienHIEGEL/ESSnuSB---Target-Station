# BuilFunctions/Quad.py

from .Element import Element
import numpy as np

class Quad(Element): 
    def __init__(self, config):
        super().__init__()
        self.bodies     += f'*\n* ----- Quad bodies ----- \n*\n'
        self.regions    += f'*\n* ----- Quad regions ----- \n*\n'
        self.materials  += f'*\n* ----- Quad materials ----- \n*\n'
        self.transforms += f'\n*\n* ----- Quad transforms ----- \n*\n'
        self.lattices   += f'\n*\n* ----- Quad lattice ----- \n*\n'

        quad = config["Quad"]
        self.model = True # if we want to create the model in the parking
        
        self.N = quad["N"]
        self.zquad1 = quad["zquad1"]
        # Helium
        self.volw = quad["volw"]
        self.volR = quad["volR"]
        # Iron
        self.few = quad["few"]
        self.feR = quad["feR"]
        # Vaccum
        self.vacw = quad["vacw"]
        self.vacR = quad["vacR"]

        if self.model:
            self.z0 = quad["zPark"]
            self.x0 = quad["xPark"]
            self.y0 = quad["yPark"]

        self.theta = config["t2r"]["theta"]
        self.t2rw  = config["t2r"]["width"]
        self.DRAW = quad["DRAW"]
        
    def create_cards(self):
        if not self.DRAW: return
        ###########################################################
        ######################### BODIES ##########################
        ########################################################### 

        zfe = (self.volw - self.few) * 0.5 # centered
        zvac= (self.volw - self.vacw) * 0.5
        
        self.bodies += f"""RCC mqavol   {self.x0} {self.y0} {self.z0} 0.0 0.0 {self.volw} {self.volR}
RCC mqafe    {self.x0} {self.y0} {self.z0 + zfe} 0.0 0.0 {self.few} {self.feR}
RCC mqavac   {self.x0} {self.y0} {self.z0 + zvac} 0.0 0.0 {self.vacw} {self.vacR}
"""

        self.regions += f"""
MQAVOL       5 +mqavol -mqafe
MQAFE        5 +mqafe -mqavac
MQAVAC       5 +mqavac
"""
        Xf_t2r = 800.0 #  [cm] Reference because we know this point -> written by t2r.py - compute_transformation method
        Yf_t2r = -250.0 # [cm] Reference because we know this point -> written by t2r.py - compute_transformation method

        # dx : espace entre les murs de la t2r et les quad
        dx = (((self.t2rw - 2*self.volR) / 2)*1e-2) / np.sin(np.radians(np.abs(self.theta)))
        Xf0 = Xf_t2r + (dx * 100) # Left Corner point, but we want the middle of the quad point
        Yf0 = Yf_t2r              # Left Corner point, but we want the middle of the quad point
        d   = 20                  #20 cm d'espacement entre quad
        
        D   = self.volw + d       # shift à faire entre chaque quad
        
        for i in range(self.N):
            dx_ = (self.zquad1 + i * D) * np.cos(np.radians(np.abs(self.theta)))
            dy_ = (self.zquad1 + i * D) * np.sin(np.radians(np.abs(self.theta)))

            Xf = Xf0 + dx_
            Yf = Yf0 - dy_

            self.Xf = Xf
            self.Yf = Yf
            Trx, Try = self.compute_transformation(self.volR, self.theta, Xf, Yf)

            self.transforms += f"""ROT-DEFI         200         0{-self.theta:>10}{Try:>10.4f}{0:>10}{Trx:>10.4f}trquad{i+1}\n"""
 
            self.bodies += f"""$start_transform trquad{i+1}
RCC quad{i+1}    0.0 0.0 {self.z0} 0.0 0.0 {self.volw} {self.volR}
$end_transform 
"""
            self.regions += f"""QUAD{i+1}        5 +quad{i+1}\n"""
            self.lattices += f"""LATTICE        QUAD{i+1}                        quadn{i+1}                    -trquad{i+1}\n"""
        ###########################################################
        ###################### MAT & ASSIGNMA #####################
        ###########################################################
        
        self.materials += f"""ASSIGNMA      HELIUM    MQAVOL
ASSIGNMA        IRON     MQAFE                            
ASSIGNMA      VACUUM    MQAVAC
"""




    def compute_transformation(self, width, theta, Xf, Yf):
        '''
        compute the translation parameter to be applied before rotation of theta degrees. 
        
        width : Radius of the element (to get X0, Z0)
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
        width = 2 * width*1e-2
        theta = np.deg2rad(theta)
        Xf = Xf*1e-2
        Yf = Yf*1e-2
        
        # initial coordinate of the central point
        #X0 = (self.z0 + 0.5*self.volw) * 1e-2
        X0 = (self.z0) * 1e-2
        Y0 = 0 -width/2

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
                if card.startswith("DT           5 +dt -exparea"):
                    card += " -quad1 -quad2 -quad3"
                    lines[i] = card
                if card.startswith("PARKING      5"):
                    card += " -mqavol"
                    lines[i] = card
                if card.startswith("TTLINE       5"):
                    card += " -quad1 -quad2 -quad3 -quad4 -quad5 -quad6 -quad7"
                    lines[i] = card
            return "\n".join(lines) + "\n"   

        