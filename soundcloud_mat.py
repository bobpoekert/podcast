#!/usr/bin/env python3
# coding: utf-8
from scipy.sparse import csr_matrix
import gzip
import numpy as np
from sklearn.decomposition import TruncatedSVD
import pickle
from MulticoreTSNE import MulticoreTSNE as TSNE
from multiprocessing import cpu_count
import falconn
import threading
from os.path import join as path_join
from os.path import exists as path_exists

def shade(points, width=1280, height=1024, bounds=None):

    _width = width
    width = height
    height = _width

    if bounds is None:
        max_x = np.max(points[:, 0])
        max_y = np.max(points[:, 1])
        min_x = np.min(points[:, 0])
        min_y = np.min(points[:, 1])
    else:
        max_x, max_y, min_x, min_y = bounds

    bin_width = (max_x - min_x) / width
    bin_height = (max_y - min_y) / height

    aggs = np.zeros((int(width), int(height)), dtype=np.uint64)

    data_idx = 0
    max_data_idx = points.shape[0]

    while data_idx < max_data_idx:
        x, y = points[data_idx]
        res_x = x / bin_width
        res_y = y / bin_height
        aggs[int(res_x), int(res_y)] += 1
        data_idx += 1

    aggs = np.log(aggs)


    normed_aggs = aggs / np.max(aggs)
    return normed_aggs

def color(img, cmap='Spectral'):
    from matplotlib.cm import get_cmap
    colormap = get_cmap(cmap)
    return Image.fromarray((colormap(img) * 255).astype(np.uint8), mode='RGBA')

def plot(points, width=1280, height=1024, cmap='Spectral'):
    return color(shade(points, width, height), cmap)

def csr_tofile(mat, fname):
    with open(fname, 'wb') as outf:
        pickle.dump(mat, outf)

def csr_fromfile(fname):
    with open(fname, 'rb') as inf:
        return pickle.load(inf)

class LSAFactorizer(object):

    def __init__(self, dirname, dims=128):
        self.dims = dims
        self.dirname = dirname
        self._ids = None
        self._id_dict = None
        self._mat = None
        self.squisher = TruncatedSVD(n_components=dims)
        self._sample_idxes = None
        self._mat_sample = None
        self._is_fit = False
        self._transformed = None
        self._squished_2d = None

    @property
    def tsv_fname(self):
        return path_join(self.dirname, 'soundcloud_graph.tsv.gz')

    def tsv(self):
        return gzip.open(self.tsv_fname, 'r')

    @property
    def ids(self):
        if self._ids is None:
            id_ids = set([])
            with self.tsv() as inf:
                for row in inf:
                    for col in row.strip().split():
                        id_ids.add(col)

            id_ids = list(id_ids)
            id_ids.sort()

            self._ids = id_ids

        return self._ids

    @property
    def id_dict(self):
        if self._id_dict is None:
            self._id_dict = dict(zip(self.ids, range(len(self.ids))))
        return self._id_dict

    def init_mat(self):
        prev_id = None
        rows = []
        cols = []

        with self.tsv() as inf:
            for row in inf:
                parts = row.strip().split()
                try:
                    row, col = parts
                except:
                    print parts
                    continue
                row_id = self.id_dict[row]
                col_id = self.id_dict[col]
                rows.append(row_id)
                cols.append(col_id)

        rows = np.array(rows)
        cols = np.array(cols)

        assert rows.shape == cols.shape

        vals = np.ones(rows.shape)

        self._mat = csr_matrix((vals, (cols, rows)))

    @property
    def mat_fname(self):
        return path_join(self.dirname, 'soundcloud_graph.npy')

    @property
    def mat(self):
        if self._mat is None:
            if path_exists(self.mat_fname):
                self.load_mat(self.mat_fname)
            else:
                self.init_mat()
                self.save_mat(self.mat_fname)
        return self._mat

    def load_mat(self, fname):
        with open(fname, 'rb') as inf:
            self._mat = pickle.load(inf)

    def save_mat(self, fname):
        assert self._mat is not None, 'call init_mat() first'
        with open(fname, 'wb') as outf:
            pickle.dump(self._mat, outf)

    @property
    def mat_sample_idxes_fname(self):
        return path_join(self.dirname, 'soundcloud_sample_idxes.npy')

    def save_mat_sample_idxes(self, fname):
        assert self._mat_sample_idxes is not None
        self._mat_sample = csr_tofile(self._mat_sample_idxes, fname)

    def load_mat_sample_idxes(self, fname):
        return csr_fromfile(fname)

    @property
    def sample_idxes(self):
        if not hasattr(self, '_mat_sample_idxes') or self._mat_sample_idxes is None:
            if path_exists(self.mat_sample_idxes_fname):
                self._mat_sample_idxes = self.load_mat_sample_idxes(self.mat_sample_idxes_fname)
            else:
                self._mat_sample_idxes = np.random.choice(self.mat.shape[0], 1000000, replace=False)
                self.save_mat_sample_idxes(self.mat_sample_idxes_fname)
        return self._mat_sample_idxes

    @property
    def mat_sample(self):
        return self.mat[self.sample_idxes]


    def fit(self):
        self.squisher.fit(self.mat_sample)
        self._is_fit = True

    @property
    def squished_fname(self):
        return path_join(self.dirname, 'soundcloud_svd_128.npy')

    @property
    def squished(self):
        if self._transformed is None:
            if path_exists(self.squished_fname):
                self.load_squished(self.squished_fname)
            elif self._is_fit:
                self._transformed = self.squisher.transform(self.mat)
                self.save_squished(self.squished_fname)
            else:
                raise StandardError('call fit()')
        return self._transformed

    def save_squished(self, fname):
        assert self._transformed is not None, 'call transform() first'
        self._transformed.tofile(fname)

    def load_squished(self, fname):
        self._transformed = np.fromfile(fname).reshape((-1, self.dims))
        self._is_fit = True

    @property
    def suqished_sample(self):
        return self.squished[self.sample_idxes]

    @property
    def squished_2d_fname(self):
        return path_join(self.dirname, 'soundcloud_2d_squished_128.npy')

    @property
    def squished_2d(self):
        if self._squished_2d is None:
            if path_exists(self.squished_2d_fname):
                self.load_squished_2d(self.squished_2d_fname)
            else:
                tsne = TSNE(n_jobs=cpu_count())
                self._squished_2d = tsne.fit_transform(self.squished_sample)
                self.save_squished_2d(self.squished_2d_fname)

    def save_squished_2d(self, fname):
        with open(fname, 'w') as outf:
            self.squished_2d.tofile(outf)

    def load_squished_2d(self, fname):
        self._squished_2d = np.fromfile(fname).reshape((-1, 2))

    def plot(self):
        return plot(self.squished_2d)


class UserDB(object):

    def __init__(self, db_fname):
        self.db = sqlite3.connect(db_fname)

    def get_col(self, col, _ids):
        start = 0
        batch_size = 1000
        while start < len(_ids):
            cur = self.db.cursor()
            inp_batch = _ids[start:(start + batch_size)]
            q = 'select id, %s from users where id in (%s)' % (col, ','.join(str(v) for v in inp_batch))
            cur.execute(q)
            res = dict(cur)
            for k in inp_batch:
                try:
                    yield res[int(k)]
                except KeyError:
                    pass
            start += batch_size

    def description(self, _ids):
        return self.get_col('description', _ids)

    def get_ids(self, where):
        cur = self.db.cursor()
        cur.execute('select id from users where %s' % where)
        return [v[0] for v in cur]

    def query(self, *args):
        cur = db.cursor()
        return list(cur.execute(*args))

def distances(point, mat):
    dx = mat[:, 0] - point[0]
    dy = mat[:, 1] - point[1]
    return np.sqrt(dx*dx + dy*dy)

class LSAIndex(object):

    def __init__(self, lsa):
        "lsa is an LSAFactorizer"
        self.lsa = lsa
        self.thread_local = threading.local()
        self.init_index()

    def init_index(self):
        params = falconn.get_default_parameters(self.lsa.squished.shape[0], self.lsa.squished.shape[1])
        index = falconn.LSHIndex(params)
        index.setup(self.lsa.squished)
        self.index = index

    @property
    def cursor(self):
        if not hasattr(self.local, '_cursor'):
            self.local._cursor = self.index.construct_query_object()
        return self.local._cursor

    def get_neighbors(self, ids):
        idxes = [self.lsa.id_dict[str(_id).encode('utf-8')] for _id in ids]
        points = self.lsa.squished[idxes]
        neighbor_idxes = []
        neighbor_distances = []
        for point in points:
            res = self.cursor.find_near_neighbors(point, 6)
            neighbor_idxes.append(res)
            neighbor_distances.append(distances(point, squished[res]))
        neighbor_ids = [id_ids[v] for vv in neighbor_idxes for v in vv]
        return (neighbor_ids, neighbor_distances)


def get_neighbors(lsa_index, db, where):
    ids = db.get_ids(where)
    neighbor_ids, neighbor_distances = lsa_index.get_neighbors(ids)
    reutrn (list(db.get_col('permalink', [v.decode('utf-8') for v in neighbor_ids])), neighbor_distances)


if __name__ == '__main__':
    import sys
    dirname = sys.argv[1]
    fact = LSAFactorizer(dirname)
    fact.fit()
    fact.squished
