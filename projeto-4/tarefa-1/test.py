import os
import numpy as np
import matplotlib.pyplot as plt

with open("test.out", 'r') as file:
    binary_strings = [line.strip() for line in file]
    
binary_array = np.array([[int(bit) for bit in row] for row in binary_strings])

plt.rcParams['image.cmap'] = 'binary'
fig, ax = plt.subplots()

ax.imshow(binary_array, cmap='binary', interpolation='nearest',aspect='equal')
ax.tick_params(left = False, right = False, labelleft = False, labelbottom = False, bottom = False)
ax.set_xticks([])
ax.set_yticks([])
plt.axis('off')
plt.savefig("test.jpeg", bbox_inches='tight', dpi=1200, pad_inches = 0)
