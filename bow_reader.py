import numpy as np
import gzip
from queue import Queue
import threading
import os

vec_dims = 128
n_words = 9995

class BOWReader(object):

    def __init__(self, infname, batch_size=4096):
        self.infname = infname
        self.batch_size = batch_size
        self.file_size = os.stat(infname).st_size
        self.inf = gzip.open(infname, 'r')
        self.outp_queue = Queue(maxsize=2)

    def __len__(self):
        return self.file_size / self.row_size

    @property
    def vec_size(self):
        return vec_dims * 8

    @property
    def bag_size(self):
        return n_words * 4

    @property
    def row_size(self):
        return self.vec_size + self.bag_size

    def get_batch(self):
        bags = np.zeros((self.batch_size, n_words), dtype=np.uint32)
        vecs = np.zeros((self.batch_size, vec_dims), dtype=np.float64)

        for i in range(self.batch_size):
            blob = self.inf.read(self.row_size)
            bag = np.frombuffer(blob[:self.bag_size], dtype=np.uint32)
            vec = np.frombuffer(blob[self.bag_size:], dtype=np.float64)

            bags[i] = bag
            vecs[i] = vec

        return bags, vecs

    def batch_worker(self):
        while 1:
            try:
                self.outp_queue.put(self.get_batch())
            except:
                self.outp_queue.put('done')
                break

    def get_batches(self):
        thread = threading.Thread(target=self.batch_worker)
        thread.daemon = True
        thread.start()
        while 1:
            res = self.outp_queue.get()
            if res == 'done':
                break
            yield res
