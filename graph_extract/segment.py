import sys
sys.path.append('./vendor')

import _segment

def segment(*args, **kwargs):
    return _segment._felzenszwalb_cython(*args, **kwargs)