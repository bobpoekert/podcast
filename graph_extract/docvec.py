from random import shuffle
from gensim.models import Doc2Vec
from gensim.models.doc2vec import TaggedDocument
import gensim
from multiprocessing import cpu_count
import sys
import numpy as np
import sklearn.cluster as cl
import threading

infname = sys.argv[1]
model_fname = sys.argv[2]
vecs_fname = sys.argv[3]

n_cores = cpu_count()

def runpartition(insize):
    def _(fn):
        threads = []
        part_size = int(insize / n_cores)
        for off in range(0, insize, part_size):
            thread = threading.Thread(target=fn, args=(off, off+part_size))
            thread.daemon = True
            thread.start()
            threads.append(thread)

        for thread in threads:
            thread.join()

        return fn
    return 

docs = []
with open(infname, 'r') as inf:
    for row in inf:
        k, text = row.strip().split('\t')
        words = text.split()
        docs.append(TaggedDocument(words, [int(k)]))

n_cores = cpu_count()

assert gensim.models.doc2vec.FAST_VERSION > -1

model = Doc2Vec(dm=0, vector_size=100, negative=5, hs=0, min_count=2, sample=0,
                epochs=20, workers=n_cores)

for i in range(10):
    shuffle(docs)
    model.train(docs, total_examples=len(docs), epochs=model.epochs)

model.save(model_fname)

embedding_vecs = np.zeros((len(docs), 100))


@runpartition(len(docs))
def infer(start, end):
    chunk = docs[start:end]
    for idx, row in enumerate(chunk):
        embedding_vecs[(idx + start), :] = model.infer_vector(row.words)


n_clusters = 4096
clusterer = cl.MiniBatchKMeans(n_clusters=n_clusters, batch_size=5000)


@runpartition(embedding_vecs.shape[0])
def cluster(start, end):
    clusterer.partial_fit(embedding_vecs[start:end])


text_clusters = clusterer.predict(embedding_vecs)
doc_hashes = np.array([v.tags[0] for v in docs])

sort_idxes = np.argsort(doc_hashes)
text_clusters = text_clusters[sort_idxes]
doc_hashes = doc_hashes[sort_idxes]

collab_clusters_bin = np.fromfile('cluster_ids.npy', dtype=np.int64)
collab_clusters_size = int(collab_clusters_bin.shape[0] / 2)
collab_hashes = collab_clusters_bin[:collab_clusters_size]
collab_clusters = collab_clusters_bin[collab_clusters_size:]

unique_collab_clusters = np.unique(collab_clusters)
n_collab_clusters = unique_collab_clusters.shape[0]

collab_cluster_dists = np.zeros((n_collab_clusters, n_clusters))


@runpartition(n_clusters)
def _(start, end):
    for cluster_id in unique_collab_clusters[start:end]:
        hashes = collab_hashes[collab_clusters == cluster_id]
        text_idxes = np.searchsorted(doc_hashes, hashes)
        local_text_clusters = text_clusters[text_idxes]
        hist = np.bincount(local_text_clusters, minlength=n_clusters)
        collab_cluster_dists[cluster_id, :] = hist


collab_cluster_dists.tofile(vecs_fname)

