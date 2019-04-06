import numpy as np
import scipy.optimize as opt
from scipy.sparse import csr_matrix, coo_matrix
from autograd import grad, hessian_vector_product

def dict_vector(d, term_ids):
    res = np.zeros((len(term_ids),), dtype=np.uint64)
    for k, c in d.items():
        try:
            res[term_ids[k]] = c
        except KeyError:
            pass
    return res / np.sum(res)

def dicts_vectors(ds, term_ids):
    row_ind = []
    col_ind = []
    data = []
    for row_idx, d in enumerate(ds):
        for k, c in d.items():
            try:
                idx = term_ids[k]
            except KeyError:
                continue
            row_ind.append(row_idx)
            col_ind.append(idx)
            data.append(c)
    res = csr_matrix((data, (row_ind, col_ind)))
    return res / np.sum(res, axis=0)

class TopicModel(object):

    def __init__(self, topics):
        self.topic_dicts = topics
        terms = set([])
        for t in topics:
            terms.update(t.keys())
        terms = list(terms)
        self.terms = terms
        self.term_ids = dict(zip(terms, range(len(terms))))
        self.topics_mat = dicts_vectors(topics, self.term_ids)
        #self.topics_mat = np.array([dict_vector(v, self.term_ids) for v in topics])

    def dist_vector(self, d):
        size = 0
        idxes = []
        counts = []
        total = 0
        for k, v in d.items():
            try:
                idx = self.term_ids[k]
            except KeyError:
                continue
            size += 1
            total += v
            idxes.append(idx)
            counts.append(v)
        counts = np.array(counts) / total
        rows = np.zeros((size,), dtype=np.uint32)
        return csr_matrix((counts, (rows, idxes)), shape=(1, self.topics_mat.shape[1]))

    def divergence_mixture(self, target):
        dims = self.topics_mat.shape[0]
        diffs = np.zeros((dims,))
        for i in range(self.topics_mat.shape[0]):
            diffs[i] = np.sum(np.abs(target - self.topics_mat[i]))
        props = 1 / (diffs + 0.0000000001)
        return props / np.sum(props)

    def topic_proportions(self, target):
        initial_guess = self.divergence_mixture(target).flatten()
        return initial_guess

        def _loss(x):
            mse = np.mean((np.sum(self.topics_mat * x[:, np.newaxis], axis=0) - target)**2)
            regularizer = np.sum(x * np.log(x))
            return mse + regularizer

        print(_loss(initial_guess))

        return opt.minimize(_loss, initial_guess)
        #    jac=grad(_loss), hessp=hessian_vector_product(_loss), method='Newton-CG')

    def vectorize_dict(self, d):
        return self.topic_proportions(self.dict_vector(d))
        
