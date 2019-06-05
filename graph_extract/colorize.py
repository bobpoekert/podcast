import numpy as np
import sys
from PIL import Image

fname = sys.argv[1]
width = int(sys.argv[2])
height = int(sys.argv[3])
outfname = sys.argv[4]

mat = np.fromfile(fname, dtype=np.int64).reshape((width, height))

vals = np.unique(mat)
vals = np.sort(vals)

colors = np.random.randint(0, high=255, size=(vals.shape[0],), dtype=np.uint8)

id_mat = np.searchsorted(vals, mat)

img = np.empty((width, height, 3), dtype=np.uint8)
img[:, :, 0] = colors[id_mat]
img[:, :, 1] = 255
img[:, :, 2] = 255

Image.fromarray(img, mode='HSV').convert('RGB').save(outfname)
