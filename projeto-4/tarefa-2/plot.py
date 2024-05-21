import numpy as np
import matplotlib.pyplot as plt

def plotfunc(name):
    with open(name+'.out', 'r') as file:
        coordinates = [tuple(map(float, line.strip().split())) for line in file]

    max_abs_x = max(abs(coord[0]) for coord in coordinates)
    max_abs_y = max(abs(coord[1]) for coord in coordinates)
    grid_size = int(max(max_abs_x, max_abs_y)) + 1

    grid = np.zeros((2*grid_size+1, 2*grid_size+1))

    for x, y in coordinates:
        grid[int(y) + grid_size, int(x) + grid_size] = 1

    inverted_grid = 1 - grid

    plt.imshow(inverted_grid, cmap='gray', aspect='equal')
    plt.axis('off')
    plt.tick_params(left = False, right = False, labelleft = False, labelbottom = False, bottom = False)
    plt.savefig(name+".jpeg", bbox_inches='tight', dpi=600, pad_inches = 0)

names = ['seed-42','seed-73','seed-97']

#plotfunc('data')

for name in names:
    plotfunc(name)
