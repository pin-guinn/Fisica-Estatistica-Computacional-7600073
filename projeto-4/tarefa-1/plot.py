import os
import numpy as np
import matplotlib.pyplot as plt

rules = ['051','073','129','105','232','254']

for rule in rules:
    for prefix in ['0-','1-','R-']:
        abspath = os.path.dirname(os.path.abspath(__file__))
        path = abspath + "/rule-" + rule + "/" + prefix + "rule=" + rule + ".out"
        figname = abspath + "/rule-" + rule + "/" + prefix + "rule=" + rule + ".png"
        with open(path, 'r') as file:
            binary_strings = [line.strip() for line in file]
            
        binary_array = np.array([[int(bit) for bit in row] for row in binary_strings])

        plt.imshow(binary_array, cmap='binary', aspect='equal')
        plt.tick_params(left = False, right = False, labelleft = False, labelbottom = False, bottom = False)
        plt.savefig(figname, bbox_inches='tight', dpi=300, pad_inches = 0)
