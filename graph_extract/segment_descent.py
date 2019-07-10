from segment import make_clusters
import subprocess as sp
import sys, os
import numpy as np
from threading import Thread, Semaphore 
from multiprocessing import cpu_count
from functools import partial

pool_sem = Semaphore(value=cpu_count()*1.5)

def run(job):
    def _run():
        try:
            job()
        finally:
            pool_sem.release()
    pool_sem.acquire()
    thread = Thread(target=_run)
    thread.daemon = True 
    thread.start()
    return thread

dists_fname = sys.argv[1]
fname_2d = sys.argv[2]

mat_2d = np.fromfile(fname_2d, dtype=np.float32).reshape((-1, 2))
n_topics = mat_2d.shape[0]
topics_bbox = (
    np.min(mat_2d[:, 0]), np.min(mat_2d[:, 1]),
    np.max(mat_2d[:, 0]), np.max(mat_2d[:, 1])
)

n_levels = 5
image_size_px = (800, 800)
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
    for x_off in np.linspace(bbox[0], bbox[2]):
        for y_off in np.linspace(bbox[1], bbox[3]):
            yield (x_off, y_off, x_off + step_x, y_off + step_y)

def make_prefix(bbox):
    b = "%f_%f_%f_%f" % bbox
    return "%s_%d_%d" % (b, image_size_px[0], image_size_px[1])

def call(cmd):
    print(' '.join(cmd))
    return sp.check_call(cmd)

def partition(things, partition_size):
    return [things[start:(start + partition_size)] for start in range(0, len(things), partition_size)]

def make_raster(bbox, is_parallel=False):
    min_x, min_y, max_x, max_y = bbox
    prefix = make_prefix(bbox)
    fname = '/mnt/lappy2/tiles/%s_dists.bin' % prefix
    if not os.path.exists(fname):
        call(['./word_map.exe', dists_fname, fname_2d,
            str(image_size_px[0]), str(image_size_px[1]),
            fname,
            str(min_x), str(min_y), str(max_x),str(max_y),
            'true' if is_parallel else 'false'])

def quickshift(bbox):
    prefix = make_prefix(bbox)
    fname = '/mnt/lappy2/tiles/%s_labels.bin' % prefix
    if not os.path.exists(fname):
        labels = make_clusters('/mnt/lappy2/tiles/%s_dists.bin' % prefix, image_size_px[0], image_size_px[1])
        labels.tofile(fname)

def make_tile_tree_from_slice(part, level):
    for bbox in part:
        make_tile_tree(bbox, level=level)

def make_tile_tree(bbox, level=1):
    make_raster(bbox, is_parallel=(level == 1))
    quickshift(bbox)
    if level < n_levels:
        threads = []
        for part in slice_bboxes(bbox):
            threads.append(run(partial(make_tile_tree, part, level=level+1)))
        print('threads', len(threads))
        for thread in threads:
            thread.join()

if __name__ == '__main__':
    make_tile_tree(topics_bbox)
