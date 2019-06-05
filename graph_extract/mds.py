from sklearn.manifold import TSNE
import numpy as np
from math import sqrt

mat = np.fromfile('pairwise_clusters.bin')
n_clusters = int(sqrt(mat.shape[0]))
print(n_clusters)
mat = mat.reshape((n_clusters, n_clusters))

mat = np.maximum(mat, mat.T)
print(np.min(mat))

model = TSNE(metric='precomputed')
embedded = model.fit_transform(mat)


def centroid(points):
    sum_x = np.sum(points[:, 0])
    sum_y = np.sum(points[:, 1])
    count = points.shape[0]
    return (sum_x / count, sum_y / count)


mat_c = centroid(embedded)
dx = embedded[:, 0] - mat_c[0]
dy = embedded[:, 1] - mat_c[1]
distances = np.sqrt(dx*dx + dy*dy)

cutoff = np.std(distances) * 3
bad_points = distances > cutoff
bad_distances = distances[bad_points]
t = cutoff / bad_distances
embedded[bad_points, 0] = ((1 - t) * mat_c[0]) + (t * embedded[bad_points, 0])
embedded[bad_points, 1] = ((1 - t) * mat_c[1]) + (t * embedded[bad_points, 1])

embedded.astype(np.float32).tofile('clusters_2d.bin')
