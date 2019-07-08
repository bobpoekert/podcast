import numpy as np 
import sys

mat_dtype = np.dtype([('ks', np.int64), ('vs', np.float64)])

width = int(sys.argv[2])
height = int(sys.argv[3])

struct_mat = np.memmap(sys.argv[1], dtype=mat_dtype).reshape((width, height, -1))
ks_mat = struct_mat['ks']
vs_mat = struct_mat['vs']

max_idxes = np.argmax(vs_mat, axis=2)
print(max_idxes)
max_ks = np.zeros((width, height), dtype=np.int32)
max_ks[max_idxes.reshape((width, height))] = ks_mat[max_idxes.reshape((width, height, 1))]

print(max_ks.shape)

max_ks.astype(np.int32).tofile(sys.argv[4])