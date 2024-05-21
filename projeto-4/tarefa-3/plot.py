import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

names = ['seed-15','seed-37','seed-92']
colors = ['darkgreen', 'purple', 'darkblue']

def myplot(name, color):
    with open(name+'.out', 'r') as file:
        coordinates = [tuple(map(float, line.strip().split())) for line in file]

# Determine the size of the grid
    max_abs_x = max(abs(coord[0]) for coord in coordinates)
    max_abs_y = max(abs(coord[1]) for coord in coordinates)
    max_abs_z = max(abs(coord[2]) for coord in coordinates)
    grid_size = int(max(max_abs_x, max_abs_y, max_abs_z)) + 1

    grid = np.zeros((2*grid_size+1, 2*grid_size+1, 2*grid_size+1))

    for x, y, z in coordinates:
        grid[int(z) + grid_size, int(y) + grid_size, int(x) + grid_size] = 1

    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')

    indices = np.argwhere(grid == 1)

    ax.scatter(indices[:, 2] - grid_size, indices[:, 1] - grid_size, indices[:, 0] - grid_size, s=5, marker=',', color=color)

    plt.axis('off')
    plt.grid(b=None)
    ax.set_box_aspect([1,1,1])  # Equal aspect ratio
    plt.tick_params(left = False, right = False, bottom = False, top=False,labelleft = False, labelbottom = False,labelright=False,labeltop=False)
    plt.savefig(name+".png", dpi=300, pad_inches = 0, bbox_inches='tight')

for name, color in zip(names,colors):
    myplot(name, color)
