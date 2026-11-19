# BuildFunctions/Element.py

class Element:
    def __init__(self):
        self.bodies         = f"""\n"""
        self.regions        = f"""\n"""
        self.materials      = f"""\n"""
        self.transforms     = f"""\n"""
        self.lattices       = f"""\n"""


    def create_cards(self, config):
        pass

    def compute_transormation(self):
        pass

    def add_regions(self, cards):
        '''
        Use to add or not the regions in EXISTING regions (depending on the True/False value of the DRAW attribut
        '''
        return cards