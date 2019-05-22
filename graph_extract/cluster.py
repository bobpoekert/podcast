#!/usr/bin/env OPENBLAS_NUM_THREADS=1 python3
import numpy as np
from scipy.sparse import csr_matrix
from sklearn.decomposition import TruncatedSVD
import sklearn.cluster as cl
import sys
from numba import jit
import os
import threading
from multiprocessing import cpu_count


inp = np.fromfile('graph.bin', dtype=np.dtype('>i4')).reshape((-1, 3))

keys = np.unique(np.concatenate((np.unique(inp[:, 0]), np.unique(inp[:, 1]))))
print(keys.shape, inp.shape)
keys = np.sort(keys)

print(np.min(keys), np.max(keys))

print('loaded')

vals = inp[:, 2].astype(np.float32)
min_val = np.min(vals)
max_val = np.max(vals)
vals = (min_val + vals) / (min_val + max_val)

ids_left = np.searchsorted(keys, inp[:, 0])
ids_right = np.searchsorted(keys, inp[:, 1])


print('sorted')

#sample_size = 100000
#sample_idxes = np.random.choice(id_left.shape[0], sample_size, replace=False)

#sample_mat = csr_matrix((vals[sample_idxes], (ids_left[sample_idxes], ids_right[sample_idxes])), shape=(sample_size, sample_size))
affinity_mat = csr_matrix((vals, (ids_left, ids_right)), shape=(keys.shape[0], keys.shape[0]))

print('sparse')

if os.path.exists('eigenvecs.npy'):
    eigenvecs = np.fromfile('eigenvecs.npy', dtype=np.float32).reshape((-1, 128))
else:

    svd = TruncatedSVD(n_components=128)
    svd.fit(affinity_mat)

    eigenvecs = svd.transform(affinity_mat)

    eigenvecs.astype(np.float32).tofile('eigenvecs.npy')

print('embedded')

#bandwidth = cl.estimate_bandwidth(eigenvecs, n_samples=10000, n_jobs=-1)

#ms = cl.MeanShift(bandwidth=bandwidth, bin_seeding=True, n_jobs=-1)
#ms.fit(eigenvecs)

n_clusters = 1024
clusterer = cl.MiniBatchKMeans(n_clusters=1024, batch_size=5000)

workers = []
n_workers = cpu_count()
partition_size = int(eigenvecs.shape[0] / n_workers)
partition_size -= partition_size % 1024

for off in range(0, eigenvecs.shape[0], partition_size):
    thread = threading.Thread(target=clusterer.partial_fit, args=(eigenvecs[off:(off + partition_size)],))
    thread.daemon = True
    thread.start()
    workers.append(thread)

for worker in workers:
    worker.join()

print('clustered')

labels = clusterer.predict(eigenvecs)

np.concatenate([keys, labels]).astype(np.int32).tofile('cluster_ids.npy')
