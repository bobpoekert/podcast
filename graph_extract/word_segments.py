import segment
import numpy as np 
import sys

mat_dtype = np.dtype([('ks', np.int64), ('vs', np.float64)])

width = int(sys.argv[2])
height = int(sys.argv[3])

struct_mat = np.fromfile(sys.argv[1], dtype=mat_dtype).reshape((width, height, -1))
ks_mat = struct_mat['ks']
vs_mat = struct_mat['vs']

segmentation = segment._felzenszwalb_cython(ks_mat, vs_mat, min_size=5)
segmentation.astype(np.int32).tofile(sys.argv[4])

segment_ids = np.unique(segmentation)

segment_word_dists = np.zeros((segment_ids.shape[0], ks_mat.shape[-1]))

for segment_id in segment_ids:
    mask = (segmentation == segment_id)
    xs, ys = np.nonzero(mask)
    ks = ks_mat[xs, ys, :]
    vs = vs_mat[xs, ys, :]
    ks_uniq = np.unique(ks)
    for i, k in enumerate(ks_uniq):
        segment_word_dists[segment_id, i] = np.sum(vs[ks == k])

segment_word_dists.astype(np.float64).tofile(sys.argv[5])