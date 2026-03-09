# BuilFunctions/__init__.py

def generate_inp_file(fname, cards, verbose=True):
    '''
    Generates input file:
    
    fname: (str) Input file name
    cards: (str) String where the cards are defined
    '''
    with open("Others_inp/" + fname, 'w', newline='\n') as f:
        for line in cards.split('\n'):
            f.write(line.replace('\r\n', '\n').replace('\r','') + '\n')
    if verbose : 
        print(f' --- created file {fname} with {len(cards.split("\n"))} lines')
    return