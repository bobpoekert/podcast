import numpy as np
import mxnet as mx
from mxnet import nd, autograd, gluon, gpu
import struct
import gzip
from sklearn.model_selection import train_test_split
from collections import namedtuple
import os
from mmap import mmap
import threading
from queue import Queue
from numba import jit

BOWRow = namedtuple('BOWRow', 'user_id bag vec')

vec_dims = 128
n_words = 10098

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
        return 8 + self.vec_size + self.bag_size

    def get_batch(self):
        user_ids = np.zeros((self.batch_size,), dtype=np.uint64)
        bags = np.zeros((self.batch_size, n_words), dtype=np.uint32)
        vecs = np.zeros((self.batch_size, vec_dims), dtype=np.float64)

        for i in range(self.batch_size):
            while 1:
                try:
                    blob = self.inf.read(self.row_size)
                    break
                except:
                    self.inf.close()
                    self.inf = gzip.open(self.infname, 'r')
            user_id = struct.unpack('<Q', blob[:8])[0]
            bag = np.frombuffer(blob[8:(8 + self.bag_size)], dtype=np.uint32)
            vec = np.frombuffer(blob[(8 + self.bag_size):], dtype=np.float64)

            user_ids[i] = user_id
            bags[i] = bag
            vecs[i] = vec

        return user_ids, bags, vecs

    def batch_worker(self):
        while 1:
            self.outp_queue.put(self.get_batch())

    def get_batches(self):
        thread = threading.Thread(target=self.batch_worker)
        thread.daemon = True
        thread.start()
        while 1:
            yield self.outp_queue.get()


class TextEmbedding(gluon.Block):

    def __init__(self, **kwargs):
        super(TextEmbedding, self).__init__(**kwargs)
        with self.name_scope():
            self.input = gluon.nn.Dense(n_words, activation='relu')
            self.input_squish_1 = gluon.nn.Dense(int(n_words / 2), activation='relu')
            self.input_squish_2 = gluon.nn.Dense(int(n_words / 4), activation='relu')
            self.input_squish_3 = gluon.nn.Dense(512, activation='relu')
            self.outp = gluon.nn.Dense(vec_dims + n_words)

    def forward(self, x):
        x = self.input(x)
        x = self.input_squish_1(x)
        x = self.input_squish_2(x)
        x = self.input_squish_3(x)
        x = self.outp(x)
        return x

def fit_text_embedding(data_fname, params_fname, ctx, epochs):
    net = TextEmbedding()

    print(net.collect_params())

    if os.path.exists(params_fname):
        net.load_parameters(params_fname, ctx=ctx)
    else:
        net.collect_params().initialize(mx.init.Xavier(), ctx=ctx)

    trainer = gluon.Trainer(net.collect_params(), 'nag', {'learning_rate':0.1})

    epoch = 0

    reader = BOWReader(data_fname, batch_size=1000)

    l1_loss = gluon.loss.HuberLoss()

    holdout_size = 100

    try:
        cumulative_loss = 0
        for user_ids, bags, vecs in reader.get_batches():
            bags_train, bags_test, vecs_train, vecs_test = \
                    train_test_split(bags, vecs, test_size=0.01)

            bags_train = nd.array(bags_train).as_in_context(ctx)
            bags_test = nd.array(bags_test).as_in_context(ctx)
            vecs_train = nd.array(vecs_train).as_in_context(ctx)
            vecs_test = nd.array(vecs_test).as_in_context(ctx)

            target_train = nd.concat(bags_train, vecs_train, dim=1)
            target_test = nd.concat(bags_test, vecs_test, dim=1)

            for i in range(100):
                with autograd.record():
                    outp = net(bags_train)
                    loss = l1_loss(target_train, outp)
                loss.backward()
                trainer.step(bags_train.shape[0])

                loss_scalar = nd.sum(loss).asscalar()

                cumulative_loss += loss_scalar

                test_output = net(bags_test)

                test_loss = l1_loss(target_test, test_output)
                test_loss = nd.sum(test_loss).asscalar()


                print('Epoch: %d, Train loss: %s, Test loss: %s, Cumulative loss: %s' % (
                    epoch, loss_scalar / target_train.shape[0], test_loss / target_test.shape[0], cumulative_loss / epoch))

            epoch += 1
            if epoch > epochs:
                break

    finally:
        net.save_parameters(params_fname)



if __name__ == '__main__':
    import sys
    data_fname = sys.argv[1]
    weights_fname = sys.argv[2]
    if len(sys.argv) > 3:
        epochs = int(sys.argv[3])
    else:
        epochs = 1000

    #ctx = [gpu(0), gpu(1), gpu(2)]
    ctx = gpu(1)

    fit_text_embedding(data_fname, weights_fname, ctx, epochs)

