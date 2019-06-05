import numpy as np
import sklearn.cluster as cl
from scipy.sparse import csr_matrix
import sys
import threading

raw_regular_clusters = np.fromfile('cluster_ids.npy', dtype=np.int64)
n_rows = int(raw_regular_clusters.shape[0] / 2)
regular_clusters = np.transpose([raw_regular_clusters[:n_rows], raw_regular_clusters[n_rows:]])


mat_type = np.dtype([('doc_id', np.int64), ('word_id', np.int64), ('prob', np.float32)])


def load_mat(fname):
    m = np.fromfile(fname, dtype=mat_type)
    return csr_matrix((m['prob'], (m['doc_id'], m['word_id'])))

infnames = sys.argv[1:]

raw_mats = [np.fromfile(fname, dtype=mat_type) for fname in infnames]
uniq_doc_ids = np.unique(np.concatenate([v['doc_id'] for v in raw_mats]))
mats = [csr_matrix((m['prob'], (np.searchsorted(uniq_doc_ids, m['doc_id']), m['word_id']))) for m in raw_mats]


n_clusters = 4096

clusterer = cl.MiniBatchKMeans(n_clusters=n_clusters, batch_size=5000)

workers = []

for mat in mats:
    thread = threading.Thread(target=clusterer.partial_fit, args=(mat,))
    thread.daemon = True
    thread.start()
    workers.append(thread)

for worker in workers:
    worker.join()

cluster_results = []
workers = []

def get_cluster_ids(mat):
    doc_ids, _ = mat.nonzero()
    uniq_doc_idxes = np.concatenate([(True,), (doc_ids[:-1] != doc_ids[1:])])
    uniq_doc_ids = doc_ids[uniq_doc_idxes]
    cluster_ids = clusterer.transform(mat)
    assert uniq_doc_ids.shape == cluster_ids.shape
    cluster_results.append((uniq_doc_ids, cluster_ids))

for mat in mats:
    thread = threading.Thread(target=get_cluster_ids, args=(mat,))
    thread.daemon = True
    thread.start()
    workers.append(thread)

for worker in workers:
    worker.join()

row_ids = np.concatenate([v[0] for v in cluster_results])
row_hashes = uniq_doc_ids[row_ids]
cluster_ids = np.concatenate([v[1] for v in cluster_results])

def searchsorted_filter(a, b):
    res = np.searchsorted(a, b)
    return res[a[res] == b]

def normalize(v):
    return v / np.sum(v)

uniq_hashes = np.intersect1d(row_hashes, regular_clusters[:, 0])

join_bow_ids = searchsorted_filter(uniq_hashes, row_hashes)
join_regular_ids = searchsorted_filter(uniq_doc_ids, regular_clusters[:, 0])

join_bow = cluster_ids[join_bow_ids]
join_regular = regular_clusters[join_bow_ids, 1]

max_bow = np.amax(join_bow)
max_regular = np.amax(join_regular)

tree = [normalize(np.bincount(join_regular[join_bow == i], minlength=max_bow+1)) for i in range(max_regular)]
for leaf in tree:
    assert leaf.shape[0] == max_bow+1

import struct
tree = np.array(tree).astype(np.float64)
with open('bow_cluster_dists.npy_width.bin', 'w') as outf:
    outf.write(struct.pack('>i', tree.shape[1]))
tree.tofile('bow_cluster_dists.npy')