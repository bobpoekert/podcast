import numpy as np
import sys

width = int(sys.argv[1])
height = int(sys.argv[2])

words_by_hash = {}
with open('words.txt', 'r') as inf:
    for row in inf:
        try:
            h, k = row.strip().split('\t')
        except:
            sys.stderr.write(row)
            continue
        words_by_hash[int(h)] = k


mat = np.fromfile('word_map.bin', dtype=np.int64).reshape((width, height))

mat_vals = np.unique(mat)

for idx in mat_vals:
    try:
        print('%d\t%s' % (idx, words_by_hash[idx]))
    except KeyError:
        pass
