import segment
import numpy as np 
import sys

mat_dtype = np.dtype([('ks', np.int64), ('vs', np.float64)])

width = int(sys.argv[2])
height = int(sys.argv[3])

struct_mat = np.fromfile(sys.argv[1], dtype=mat_dtype).reshape((width, height, -1))
ks_mat = struct_mat['ks']
vs_mat = struct_mat['vs']

segmentation = segment.segment(ks_mat, vs_mat, scale=5000, min_size=1, weight=1000, mean_kernel_size=(100,100))
print(segmentation.shape)
segmentation.astype(np.int32).tofile(sys.argv[4])

if False:
    segment_ids = np.unique(segmentation)

    print(ks_mat.shape, segmentation.shape)

    segment_word_dists = np.zeros((segment_ids.shape[0], ks_mat.shape[-1]))
    print(segment_word_dists.shape)
    print(segment_ids.shape)

    for segment_idx, segment_id in enumerate(segment_ids):
        mask = (segmentation == segment_id)
        xs, ys = np.nonzero(mask)
        ks = ks_mat[ys, xs, :]
        vs = vs_mat[ys, xs, :]
        ks_uniq = np.unique(ks)
        local_dist = np.zeros(ks_uniq.shape)
        for idx, k in enumerate(ks_uniq):
            local_dist[idx] = np.sum(vs[ks == k])
        sort_idxes = np.argsort(local_dist)
        top_100 = sort_idxes[-100:]
        top_ks = ks_uniq[top_100]
        segment_word_dists[segment_idx, :(top_ks.shape[0])] = top_ks

        segment_word_dists.astype(np.float64).tofile(sys.argv[5])