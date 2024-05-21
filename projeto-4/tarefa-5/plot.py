import numpy as np
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap

names = ['seed-14','seed-37','seed-88']

def myplot(name):
    with open(name+'.out', 'r') as file:
        data = [[int(val) for val in line.strip().split()] for line in file]

    data_array = np.array(data)

    colors = ['white', 'red', 'blue']
    cmap = ListedColormap(colors)

    plt.rcParams['image.cmap'] = 'gray'
    fig, ax = plt.subplots()

    plt.imshow(data_array, cmap=cmap, interpolation='nearest', origin='lower', vmin=0, vmax=2)
    ax.tick_params(left = False, right = False, labelleft = False, labelbottom = False, bottom = False)
    ax.set_xticks([])
    ax.set_yticks([])
    plt.axis('off')
    plt.savefig(name+".jpeg", bbox_inches='tight', dpi=1200, pad_inches = 0)


#myplot('data')
for name in names:
    myplot(name)
