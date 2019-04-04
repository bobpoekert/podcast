import numpy as np
import mxnet as mx
from mxnet import nd, autograd, gluon, gpu
from mxnet.gluon.loss import _reshape_like, _apply_weighting
import struct
import gzip
from sklearn.model_selection import train_test_split
from collections import namedtuple
import os
from mmap import mmap
import threading
from queue import Queue
from numba import jit
from bow_reader import *

BOWRow = namedtuple('BOWRow', 'user_id bag vec')


class WeightedHuberLoss(gluon.loss.Loss):
    r"""Calculates smoothed L1 loss that is equal to L1 loss if absolute error
    exceeds rho but is equal to L2 loss otherwise. Also called SmoothedL1 loss.

    .. math::
        L = \sum_i \begin{cases} \frac{1}{2 {rho}} ({label}_i - {pred}_i)^2 &
                           \text{ if } |{label}_i - {pred}_i| < {rho} \\
                           |{label}_i - {pred}_i| - \frac{{rho}}{2} &
                           \text{ otherwise }
            \end{cases}

    `label` and `pred` can have arbitrary shape as long as they have the same
    number of elements.

    Parameters
    ----------
    rho : float, default 1
        Threshold for trimmed mean estimator.
    weight : float or None
        Global scalar weight for loss.
    batch_axis : int, default 0
        The axis that represents mini-batch.


    Inputs:
        - **pred**: prediction tensor with arbitrary shape
        - **label**: target tensor with the same size as pred.
        - **sample_weight**: element-wise weighting tensor. Must be broadcastable
          to the same shape as pred. For example, if pred has shape (64, 10)
          and you want to weigh each sample in the batch separately,
          sample_weight should have shape (64, 1).
        - **dim_weights**: vector that abs(a - b) is multiplied by before we take its norm.
          For when loss in some dimensions is more imporatnt than others.

    Outputs:
        - **loss**: loss tensor with shape (batch_size,). Dimenions other than
          batch_axis are averaged out.
    """
    def __init__(self, rho=1, weight=None, batch_axis=0, dim_weights=None, **kwargs):
        super(WeightedHuberLoss, self).__init__(weight, batch_axis, **kwargs)
        self.dim_weights = dim_weights
        self._rho = rho

    def hybrid_forward(self, F, pred, label, sample_weight=None):
        label = _reshape_like(F, label, pred)
        loss = F.abs(label - pred)
        loss = loss * self.dim_weights
        loss = F.where(loss > self._rho, loss - 0.5 * self._rho,
                       (0.5/self._rho) * F.square(loss))
        loss = _apply_weighting(F, loss, self._weight, sample_weight)
        return F.mean(loss, axis=self._batch_axis, exclude=True)

class TextEmbedding(gluon.Block):

    def __init__(self, **kwargs):
        super(TextEmbedding, self).__init__(**kwargs)
        with self.name_scope():
            self.input = gluon.nn.Dense(n_words, activation='relu')
            self.input_squish_3 = gluon.nn.Dense(512, activation='relu')
            self.outp = gluon.nn.Dense(vec_dims + n_words)

    def forward(self, x):
        x = self.input(x)
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

    loss_weights = np.ones((vec_dims + n_words),)
    loss_weights[n_words:] = 20
    loss_weights = nd.array(loss_weights).as_in_context(ctx)
    l1_loss = WeightedHuberLoss(dim_weights=loss_weights)

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

            if epoch % 10 == 0:
                net.save_parameters(params_fname)

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

