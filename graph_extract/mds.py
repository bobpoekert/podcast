from sklearn.manifold import TSNE 
import numpy as np
from math import sqrt

mat = np.fromfile('pairwise_clusters.bin', dtype=np.int64)
n_clusters = int(sqrt(mat.shape[0]))
print(n_clusters)
mat = mat.reshape((n_clusters, n_clusters))

mat = np.maximum(mat, mat.T)
print(np.min(mat))

model = TSNE(metric='precomputed')
embedded = model.fit_transform(mat)

embedded.astype(np.float32).tofile('clusters_2d.bin')
