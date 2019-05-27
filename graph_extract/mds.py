from sklearn.manifold import MDS
import numpy as np
from math import sqrt

mat = np.fromfile('pairwise_distances.bin', dtype=np.int64)
n_clusters = sqrt(mat.shape[0])
mat = mat.reshape((n_clusters, n_clusters))

model = MDS(n_jobs=-1, metric='precomputed')
embedded = model.fit_transform(mat)

embedded.astype(np.float32).tofile('clusters_2d.bin')