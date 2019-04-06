#!/usr/bin/env python3.6

import zipfile
import numpy as np
import falconn
import soundcloud_mat
import sys, os
import pickle
from sklearn.cluster import MiniBatchKMeans
import threading
from queue import Queue
from multiprocessing import cpu_count

def pmap(fn, it):
    inq = Queue(maxsize=10)
    def worker():
        while 1:
            row = inq.get()
            if row is False:
                break
            fn(row)

    threads = []
    for i in range(cpu_count() - 1):
        thread = threading.Thread(target=worker)
        thread.daemon = True
        thread.start()
        threads.append(thread)

    for row in it:
        inq.put(row)
    for i in threads:
        inq.put(False)

    for thread in threads:
        thread.join()

def l1_distances(mat, vec):
    assert mat.shape[1] == vec.shape[0]
    return np.sum(np.abs(mat - vec), axis=1) / vec.shape[0]

def falconn_index(points):
    params = falconn.get_default_parameters(points.shape[0], points.shape[1])
    index = falconn.LSHIndex(params)
    index.setup(points)
    return index

class Counter(object):

    def __init__(self):
        self.val = {}

    def __getitem__(self, k):
        return self.val[k]

    def add(self, k):
        if k not in self.val:
            self.val[k] = 0
        self.val[k] += 1

    def update(self, pairs):
        for k, v in pairs:
            if k in self.val:
                self.val[k] += v
            else:
                self.val[k] = v

    def add_all(self, keys):
        for k in keys:
            if k in self.val:
                self.val[k] += 1
            else:
                self.val[k] = 1

    def merge(self, other):
        self.update(other.val.items())

class GloveVectors(object):

    def __init__(self, zip_path):
        self.load(zip_path)
        self.init_index()

    def load(zip_path):
        zf = zipfile.ZipFile(zip_path)
        fname = zf.namelist()[0]
        tokens = []
        vecs = []
        with zf.open(fname) as inf:
            for row in inf:
                parts = row.split()
                token = parts[0]
                vec = [float(v) for v in parts[1:]]
                tokens.append(token)
                vecs.append(np.array(vec))

        tokens_with_indexes = zip(tokens, range(len(tokens)))
        tokens_with_indexes.sort(key=lambda v: v[0])
        sort_indexes = [v[1] for v in tokens_with_indexes]

        self.tokens = [v[0] for v in tokens_with_indexes]
        self.vecs = np.array(vecs)[sort_indexes]
        self.token_idxes = dict(tokens_with_indexes)

    def init_index(self):
        self.ann_index = falconn_index(self.vecs)
        self.ann_cursor = self.ann_index.construct_query_object()

    def neighbors(self, query, k=10):
        idxes = self.ann_cursor.find_k_nearest_neighbors(query, k)
        vecs = self.vecs[idxes]
        tokens = [self.tokens[i] for i in idxes]
        return (tokens, vecs)

    def __getitem__(self, token):
        return self.vecs[self.token_idxes[token]]

import threading


class FitBatches(object):

    def __init__(self, clustering, batch, minibatch_size=10000):
        self.clustering = clustering
        self.batch = batch
        self.minibatch_size = minibatch_size

    def __call__(self):
        n_minibatches = int(self.batch.shape[0] / self.minibatch_size)
        for minibatch_id in range(n_minibatches):
            off = self.minibatch_size * minibatch_id
            minibatch = self.batch[off:(off + self.minibatch_size)]
            self.clustering.partial_fit(minibatch)

    @classmethod
    def fit(cls, vec_128, n_clusters=512, n_threads=None):
        threads = []
        n_threads = 12
        batch_size = int(vec_128.shape[0] / n_threads)

        clustering = MiniBatchKMeans(n_clusters)

        if n_threads is None:
            n_threads = cpu_count() / 2

        for i in range(n_threads):
            off = int(i * batch_size)
            thread = threading.Thread(target=FitBatches(clustering, vec_128[off:(off+batch_size)]))
            threads.append(thread)
            thread.daemon = True
            thread.start()

        for thread in threads:
            thread.join()

        return clustering

class Clusters(object):

    def __init__(self, factorizer, cluster_centers_fname):
        self.fact = factorizer
        self.vec128 = factorizer.squished
        self.id_dict = self.fact.id_dict
        if not os.path.exists(cluster_centers_fname):
            clustering = FitBatches.fit(self.vec128)
            self.cluster_centers = clustering.cluster_centers_
            print(self.cluster_centers.shape, self.cluster_centers.dtype)
            self.cluster_centers.tofile(cluster_centers_fname)
        else:
            self.cluster_centers = np.fromfile(cluster_centers_fname).reshape((-1, 128))
            print(self.cluster_centers.shape)

    def get_cluster(self, sc_id):
        vec = self.vec128[self.id_dict[sc_id.encode('utf-8')]]
        return np.argmin(l1_distances(self.cluster_centers, vec))

    @property
    def n_clusters(self):
        return self.cluster_centers.shape[0]

class WordDistributions(object):

    def __init__(self, clusters, url_join_fname, soundcloud_ids_fname):
        self.clusters = clusters
        self.dists = None

        it_to_sc = {}
        with open(url_join_fname, 'r') as inf:
            for row in inf:
                try:
                    _, sc, it = row.strip().split()
                except:
                    continue

                    it_to_sc[it] = sc

        with open(soundcloud_ids_fname, 'r') as inf:
            for row in inf:
                sc_id, it_id = row.strip().split('\t')
                it_to_sc[it_id] = sc_id

        self.it_to_sc = it_to_sc

    def itunes_id_to_sc_id(self, itunes_id):
        itunes_id = str(itunes_id)
        res = self.it_to_sc.get(itunes_id)
        if res is None:
            return 'it:%s' % itunes_id
        else:
            return res

    def fit(self, generator):
        print(self.clusters.n_clusters)
        dists = [Counter() for i in range(self.clusters.n_clusters)]
        cluster_cache = {}
        def update(row):
            tags = row[0]
            description = row[1]
            name = row[2]
            name_raw = row[3]
            itunes_id = row[4]

            if itunes_id in cluster_cache:
                cluster = cluster_cache[itunes_id]
            else:
                try:
                    cluster = self.clusters.get_cluster(self.itunes_id_to_sc_id(itunes_id))
                except KeyError:
                    cluster = None
                cluster_cache[itunes_id] = cluster

            if cluster is None:
                return

            vocab = dists[cluster]

            if tags is not None:
                vocab.add_all(tags.split('\u0022'))

            if description is not None:
                vocab.add_all(description.split())

            if name is not None:
                vocab.add_all(name.split())
            elif name_raw is not None:
                vocab.add_all(name_raw.split())

        pmap(update, generator)
        return dists


def pg_get_itunes():
    import psycopg2

    db = psycopg2.connect('')
    cursor = db.cursor('get_text')
    cursor.scrollable = True
    cursor.execute(
            'select genres, description, name, name_raw, podacst_id from itunes_podcast_episodes where podacst_id is not null limit 1000')

    for idx, row in enumerate(cursor):
        if idx % 100 == 0:
            print(idx)
        podcast_id = row[4]
        if podcast_id is not None:
            yield tuple(row)

if __name__ == '__main__':
    import pickle
    fact = soundcloud_mat.LSAFactorizer('/mnt/lappy/soundcloud/', '/mnt/lappy/combined_graph.tsv.gz')
    print('fact')
    clusters = Clusters(fact, 'cluster_centers_128_512.npy')
    print('clusters')
    #dists = WordDistributions(clusters, '/mnt/lappy/url_join.txt', 'itunes_soundcloud_ids.txt')
    #print('dists')
    #res = dists.fit(pg_get_itunes())
    #with open('token_dists.pickle', 'wb') as outf:
    #    pickle.dump(res, outf)
