import numpy as np
import scipy.optimize as opt

def kl_divergence(p, q):
    return -np.sum(p * np.log(p / q))

def dict_mixture(topics, target):

    keys = list(target.keys())
    target_vec = np.array([target[k] for k in keys])
    target_vec = target_vec / np.sum(target_vec)

    dists = np.empty((self.n_topics, len(keys)))
    for topic_id, topic in enumerate(self.topics):
        for key_id, key in enumerate(keys):
            dists[topic_id, key_id] = topic.get(key, 0)

    dists = dists / np.sum(dists, axis=1)

    def _weight(weights):
        return weights.dot(dists)

    equality_constraint = opt.LinearConstraint(dists, target_vec, target_vec)

    def _loss(x):
        # entropy * -1
        return np.sum(x * np.log(x))

    divergences = -np.sum(dists * np.log(dists / target_vec))
    initial_vec = np.sum(divergences) / divergences

    return opt.optimize(_loss, initial_vec, method='SLSQP', constraints=equality_constraint)

def text_dist(text):
    words = text.split()
    counts = {}
    for word in words:
        if word in counts:
            counts[word] += 1
        else:
            counts[word] = 1
    return counts

def vectorize(topics, text):
    return dict_mixture(topics, text_dist(text))
