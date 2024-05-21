import numpy as np
import matplotlib.pyplot as plt

names = ['seed-15','seed-55','seed-61']

def myplot(name):
    with open(name+'.out', 'r') as file:
        coordinates = [tuple(map(float, line.strip().split())) for line in file]

    max_abs_x = max(abs(coord[0]) for coord in coordinates)
    max_abs_y = max(abs(coord[1]) for coord in coordinates)
    grid_size = int(max(max_abs_x, max_abs_y)) + 1

    grid = np.zeros((2*grid_size+1, 2*grid_size+1))

    for x, y in coordinates:
        grid[int(y) + grid_size, int(x) + grid_size] = 1

    inverted_grid = 1 - grid

    plt.rcParams['image.cmap'] = 'gray'
    fig, ax = plt.subplots()

    plt.imshow(inverted_grid, cmap='gray', aspect='equal', origin='lower')
    ax.tick_params(left = False, right = False, labelleft = False, labelbottom = False, bottom = False)
    ax.set_xticks([])
    ax.set_yticks([])
    plt.axis('off')
    plt.ylim(grid_size-0.5, 2*grid_size)
    plt.savefig(name+".jpeg", bbox_inches='tight', dpi=300, pad_inches = 0)


#myplot('data')
for name in names:
    myplot(name)
