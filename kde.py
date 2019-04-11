import falconn
import numpy as np

def falconn_index(points):
    params = falconn.get_default_parameters(points.shape[0], points.shape[1])
    index = falconn.LSHIndex(params)
    index.setup(points)
    return index

def l1_distance(m, v):
    assert m.shape[1] == v.shape[0]
    return np.sum(np.abs(m - v), axis=1) / m.shape[1]

def pairwise_distances(m):
    res = np.empty((m.shape[0], m.shape[0]))
    rot = np.roll(m, 1, axis=0)
    for i in range(m.shape[0]):
        res[i] = np.sum(np.abs(rot - m), axis=1) / m.shape[1]
        rot = np.roll(rot, 1, axis=0)
    return res

class KDE(object):

    def __init__(self, vec_width, batch_size=200):
        self.batch_size = batch_size
        self.batch_idx = 0
        self.vec_width = vec_width
        self.vec_batch = np.zeros((batch_size, vec_width))
        self.weight_batch = np.zeros((batch_size,), dtype=np.uint64)
        self.weight_all = None
        self.vec_all = None
        self.index = None
        self.cursor = None
        self._horizon = None
        self._max_distance = None

    def add_raw(self, point, weight=1):
        if self.batch_idx >= self.batch_size:
            self.flush()
        self.vec_batch[self.batch_idx] = point
        self.weight_batch[self.batch_idx] += weight
        self.batch_idx += 1

    def sample(self, size):
        if self.vec_all is None:
            vecs, weights = self.get_batch()
        else:
            vecs = self.vec_all
            weights = self.weight_all
        if vecs.shape[0] <= size:
            return (vecs, weights)
        idxes = np.random.choice(vecs.shape[0], size, replace=False)
        return vecs[idxes], weights[idxes]

    def update_max_distance(self):
        vec_sample, weight_sample = self.sample(1000)
        distances = pairwise_distances(vec_sample)
        mu = np.average(distances)
        sigma = np.var(distances)

        self._max_distance = mu - sigma

    @property
    def max_distance(self):
        if self._max_distance is not None:
            return self._max_distance
        else:
            return 1000

    def flush(self):
        if self.weight_all is None:
            self.weight_all = np.copy(self.weight_batch)
        else:
            old_sum = np.sum(self.weight_all)
            batch_sum = np.sum(self.weight_batch)
            self.weight_all = np.concatenate([self.weight_all, self.weight_batch])
            assert np.sum(self.weight_all) == (old_sum + batch_sum)
        if self.vec_all is None:
            self.vec_all = np.copy(self.vec_batch)
        else:
            self.vec_all = np.concatenate([self.vec_all, self.vec_batch])

        self.index = falconn_index(self.vec_all)
        self.cursor = self.index.construct_query_object()
        self.batch_idx = 0
        self.update_max_distance()

    def get_batch(self):
        return self.vec_batch[:self.batch_idx], self.weight_batch[:self.batch_idx]

    def get_neighbors(self, vec, threshold, return_idxes=False):
        vec_batch, weight_batch = self.get_batch()
        batch_distances = l1_distance(vec_batch, vec)
        batch_res_idxes = np.nonzero(batch_distances <= threshold)[0]

        batch_vecs = vec_batch[batch_res_idxes]
        batch_weights = weight_batch[batch_res_idxes]

        if self.index is None:
            if return_idxes:
                return (batch_vecs, batch_weights, None, batch_res_idxes)
            else:
                return (batch_vecs, batch_weights)

        idx_idxes = self.cursor.find_near_neighbors(vec, threshold)
        if len(idx_idxes) < 1:
            if return_idxes:
                return (batch_vecs, batch_weights, None, batch_res_idxes)
            else:
                return (batch_vecs, batch_weights)

        idx_vecs = self.vec_all[idx_idxes]
        idx_weights = self.weight_all[idx_idxes]
        idx_idxes = np.array(idx_idxes)

        res = (np.concatenate([idx_vecs, batch_vecs]), np.concatenate([batch_weights, idx_weights]))
        if return_idxes:
            return res + (idx_idxes, batch_res_idxes)
        else:
            return res

    def add(self, vec, weight=1):
        neighbor_vecs, neighbor_weights, idx_idxes, batch_idxes = self.get_neighbors(vec, self.max_distance, return_idxes=True)
        if neighbor_vecs.shape[0] > 0 and idx_idxes is not None:
            distances = l1_distance(neighbor_vecs, vec)
            closest = np.argmin(distances)
            if closest > idx_idxes.shape[0]:
                self.batch_weights[batch_idxes[closest - idx_idxes.shape[0]]] += weight
            else:
                self.weight_all[idx_idxes[closest]] += weight
        else:
            self.add_raw(vec, weight)

    def evaluate(self, vec):
        batch_vecs, batch_weights = self.get_batch()
        batch_distances = l1_distance(batch_vecs, vec)

        if self.index is None:
            vecs = batch_vecs
            weights = batch_weights
            distances = batch_distances
        else:
            idxes = self.cursor.find_k_nearest_neighbors(vec, 5)
            idx_vecs = self.vec_all[idxes]
            idx_weights = self.weight_all[idxes]
            vecs = np.concatenate([batch_vecs, idx_vecs])
            weights = np.concatenate([batch_weights, idx_weights])
            distances = np.concatenate([batch_distances, l1_distance(idx_vecs, vec)])

        zeros = np.nonzero(distances == 0.0)[0]
        if zeros.shape[0] > 0:
            return weights[zeros[0]]

        assert distances.shape == weights.shape
        normed_distances = distances / np.sum(distances)
        assert abs(np.sum(normed_distances) - 1.0) < 0.00000000000001
        return np.sum(weights * normed_distances)


