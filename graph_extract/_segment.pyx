#cython: cdivision=True
#cython: boundscheck=False
#cython: nonecheck=False
#cython: wraparound=False
import numpy as np
from scipy import ndimage as ndi

cimport numpy as cnp
from _ccomp cimport find_root, join_trees

from libc.stdint cimport *


cdef compute_cost(
    cnp.ndarray[cnp.int64_t, ndim=3] a_keys, cnp.ndarray[cnp.float64_t, ndim=3] a_vs,
    cnp.ndarray[cnp.int64_t, ndim=3] b_keys, cnp.ndarray[cnp.float64_t, ndim=3] b_vs,
    Py_ssize_t item_size,
    cnp.ndarray[cnp.float64_t, ndim=1] res):
    cdef Py_ssize_t max_x = a_keys.shape[0]
    cdef Py_ssize_t max_y = a_keys.shape[1]
    cdef Py_ssize_t max_j = a_keys.shape[2]
    cdef Py_ssize_t max_res = res.shape[0]

    cdef Py_ssize_t x = 0
    cdef Py_ssize_t y = 0
    cdef Py_ssize_t j = 0
    cdef Py_ssize_t res_idx = 0

    cdef double score

    cdef Py_ssize_t idx_a
    cdef Py_ssize_t idx_b

    cdef int64_t k_a
    cdef int64_t k_b

    cdef double v_a
    cdef double v_b
    cdef double delta
    cdef double score_orig

    cdef cnp.ndarray[cnp.float64_t, ndim=2] sums = np.sum(a_vs, axis=2) + np.sum(b_vs, axis=2)

    while x < max_x:
        y = 0
        while y < max_y:
            score = sums[x, y]
            score_orig = score

            idx_a = 0
            idx_b = 0

            while (idx_a < item_size) and (idx_b < item_size):
                k_a = a_keys[x, y, idx_a]
                if k_a == 0:
                    break
                k_b = b_keys[x, y, idx_b]
                if k_b == 0:
                    break
                if k_a == k_b:
                    v_a = a_vs[x, y, idx_a]
                    v_b = b_vs[x, y, idx_b]
                    delta = v_a - v_b
                    score -= (delta if delta >= 0 else -delta)
                    idx_a += 1
                    idx_b += 1
                elif k_a < k_b:
                    idx_a += 1
                else:
                    idx_b += 1

            res[res_idx] = (1.0 - (score / score_orig))
            res_idx += 1
            
            y += 1
        x += 1

    print(res_idx)

    return res[:res_idx] * 100



def _felzenszwalb_cython(image_ks, image_vs, double scale=1, sigma=0.8,
                         Py_ssize_t min_size=20):
    """Felzenszwalb's efficient graph based segmentation for
    single or multiple channels.
    Produces an oversegmentation of a single or multi-channel image
    using a fast, minimum spanning tree based clustering on the image grid.
    The number of produced segments as well as their size can only be
    controlled indirectly through ``scale``. Segment size within an image can
    vary greatly depending on local contrast.
    Parameters
    ----------
    image : (N, M, C) ndarray
        Input image.
    scale : float, optional (default 1)
        Sets the obervation level. Higher means larger clusters.
    sigma : float, optional (default 0.8)
        Width of Gaussian smoothing kernel used in preprocessing.
        Larger sigma gives smother segment boundaries.
    min_size : int, optional (default 20)
        Minimum component size. Enforced using postprocessing.
    Returns
    -------
    segment_mask : (N, M) ndarray
        Integer mask indicating segment labels.
    """

    inner_size = image_ks.shape[2]
    width = image_ks.shape[0]
    height = image_ks.shape[1]
    size = width * height

    # compute edge weights in 8 connectivity:
    down_cost = compute_cost(
        image_ks[1:, :, :], image_vs[1:, :, :],
        image_ks[:-1, :, :], image_vs[:-1, :, :],
        inner_size, np.zeros((size,))
    )
    right_cost = compute_cost(
        image_ks[:, 1:, :], image_vs[:, 1:, :],
        image_ks[:, :-1, :], image_vs[:, :-1, :],
        inner_size, np.zeros((size,))
    )
    dright_cost = compute_cost(
        image_ks[1:, 1:, :], image_vs[1:, 1:, :],
        image_ks[:-1, :-1, :], image_vs[:-1, :-1, :],
        inner_size, np.zeros((size,))
    )
    uright_cost = compute_cost(
        image_ks[1:, :-1, :], image_vs[1:, :-1, :],
        image_ks[:-1, 1:, :], image_vs[:-1, 1:, :],
        inner_size, np.zeros((size,))
    )

    cdef cnp.ndarray[cnp.float_t, ndim=1] costs = np.hstack([
    	right_cost.ravel(), down_cost.ravel(), dright_cost.ravel(),
        uright_cost.ravel()]).astype(np.float) * 2.0

    # compute edges between pixels:
    cdef cnp.ndarray[cnp.intp_t, ndim=2] segments \
            = np.arange(width * height, dtype=np.intp).reshape(height, width)
    down_edges  = np.c_[segments[1:, :].ravel(), segments[:height-1, :].ravel()]
    right_edges = np.c_[segments[:, 1:].ravel(), segments[:, :width-1].ravel()]
    dright_edges = np.c_[segments[1:, 1:].ravel(), segments[:height-1, :width-1].ravel()]
    uright_edges = np.c_[segments[:height-1, 1:].ravel(), segments[1:, :width-1].ravel()]
    cdef cnp.ndarray[cnp.intp_t, ndim=2] edges \
            = np.vstack([right_edges, down_edges, dright_edges, uright_edges])

    print(edges.shape[0], costs.shape[0], edges.shape[1], costs.shape[1])

    # initialize data structures for segment size
    # and inner cost, then start greedy iteration over edges.
    edge_queue = np.argsort(costs)
    edges = np.ascontiguousarray(edges[edge_queue])
    costs = np.ascontiguousarray(costs[edge_queue])
    cdef cnp.intp_t *segments_p = <cnp.intp_t*>segments.data
    cdef cnp.intp_t *edges_p = <cnp.intp_t*>edges.data
    cdef cnp.float_t *costs_p = <cnp.float_t*>costs.data
    cdef cnp.ndarray[cnp.intp_t, ndim=1] segment_size \
            = np.ones(width * height, dtype=np.intp)

    # inner cost of segments
    cdef cnp.ndarray[cnp.float_t, ndim=1] cint = np.zeros(width * height)
    cdef cnp.intp_t seg0, seg1, seg_new, e
    cdef float cost, inner_cost0, inner_cost1
    cdef Py_ssize_t num_costs = costs.size

    with nogil:
        # set costs_p back one. we increase it before we use it
        # since we might continue before that.
        costs_p -= 1
        for e in range(num_costs):
            seg0 = find_root(segments_p, edges_p[0])
            seg1 = find_root(segments_p, edges_p[1])
            edges_p += 2
            costs_p += 1
            if seg0 == seg1:
                continue
            inner_cost0 = cint[seg0] + scale / segment_size[seg0]
            inner_cost1 = cint[seg1] + scale / segment_size[seg1]
            if costs_p[0] < min(inner_cost0, inner_cost1):
                # update size and cost
                join_trees(segments_p, seg0, seg1)
                seg_new = find_root(segments_p, seg0)
                segment_size[seg_new] = segment_size[seg0] + segment_size[seg1]
                cint[seg_new] = costs_p[0]

        # postprocessing to remove small segments
        edges_p = <cnp.intp_t*>edges.data
        for e in range(num_costs):
            seg0 = find_root(segments_p, edges_p[0])
            seg1 = find_root(segments_p, edges_p[1])
            edges_p += 2
            if seg0 == seg1:
                continue
            if segment_size[seg0] < min_size or segment_size[seg1] < min_size:
                join_trees(segments_p, seg0, seg1)
                seg_new = find_root(segments_p, seg0)
                segment_size[seg_new] = segment_size[seg0] + segment_size[seg1]

    # unravel the union find tree
    flat = segments.ravel()
    old = np.zeros_like(flat)
    while (old != flat).any():
        old = flat
        flat = flat[flat]
    flat = np.unique(flat, return_inverse=True)[1]
    return flat.reshape((height, width))