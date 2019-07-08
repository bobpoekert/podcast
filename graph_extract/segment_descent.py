from segment import make_clusters
import subprocess as sp
import sys
import numpy as np
from threading import Thread, Semaphore 
from multiprocessing import cpu_count
from functools import partial

pool_sem = Semaphore(value=cpu_count())

def run(job):
    def _run():
        with pool_sem:
            job()
    thread = Thread(target=_run)
    thread.daemon = True 
    thread.start()

dists_fname = sys.argv[1]
fname_2d = sys.argv[2]

mat_2d = np.fromfile(fname_2d, dtype=np.float32).reshape((-1, 2))
n_topics = mat_2d.shape[0]
topics_bbox = (
    np.min(mat_2d[:, 0]), np.min(mat_2d[:, 1]),
    np.max(mat_2d(:, 0]), np.max(mat_2d[:, 1])
)

n_levels = 5
image_size_px = (1280, 1280)
tile_size_px = (300, 300)
tiles_per_screen = (
    image_size_px[0] / tile_size_px[0],
    image_size_px[1] / tile_size_px[1]
)

def slice_bboxes(bbox):
    bbox_width = bbox[2] - bbox[0]
    bbox_height = bbox[3] - bbox[1]
    step_x = bbox_width / tiles_per_screen[0]
    step_y = bbox_height / tiles_per_screen[1]
    for x_off in range(bbox[0], bbox[2], bbox_step_x):
        for y_off in range(bbox[1], bbox[3], bbox_step_y):
            yield (x_off, y_off, x_off + step_x, y_off + step_y)

def make_prefix(bbox):
    return "%f_%f_%f_%f" % bbox

def make_raster(bbox, is_parallel=False):
    min_x, min_y, max_x, max_y = bbox
    prefix = make_prefix(bbox)
    sp.check_call(['./word_map.exe', dists_fname, fname_2d,
        '%s_dists.bin' % prefix,
        tile_size_px[0], tile_size_px[1], min_x, min_y, max_x, max_y,
        'true' if is_parallel else 'false'])

def quickshift(bbox):
    prefix = make_prefix(bbox)
    labels = make_clusters('%s_dists.bin' % prefix)
    labels.tofile('%s_labels.bin' % prefix)

def make_tile_tree(bbox, level=1):
    make_raster(bbox, is_parallel=(level == 1))
    quickshift(bbox)
    if level < n_levels:
        for slice_bbox in slice_bboxes(bbox):
            run(partial(make_tile_tree, slice_bbox, level=level+1))

if __name__ == '__main__':
    make_tile_tree(topics_bbox)