import numpy as np
from math import sqrt
from scipy.sparse import csr_matrix
from sklearn.manifold import MDS

mat = np.fromfile('pairwise_clusters.bin')
n_clusters = int(sqrt(mat.shape[0]))
print(n_clusters)
mat = mat.reshape((n_clusters, n_clusters))

neighbors = np.argmin(mat, axis=1)
adjacency = csr_matrix((np.full(neighbors.shape, -1), (np.arange(neighbors.shape[0]), neighbors)))

model = MDS(dissimilarity='precomputed', n_jobs=-1)
embedded = model.fit_transform(adjacency)

embedded.astype(np.float32).tofile('clusters_2d.bin')
