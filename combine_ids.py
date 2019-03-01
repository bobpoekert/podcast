#!/usr/bin/python3

import sys
import gzip

it_fname = sys.argv[1]
join_fname = sys.argv[2]

sc_to_it = {}
it_to_sc = {}
with open(join_fname, 'r') as inf:
    for row in inf:
        try:
            _, sc, it = row.strip().split()
        except:
            continue

        if it not in it_to_sc:
            it_to_sc[it] = []
        it_to_sc[it].append(sc)

with gzip.open(it_fname, 'r') as inf:
    for row in inf:
        left, right = row.strip().split()
        if left in it_to_sc:
            left = it_to_sc[left]
        else:
            left = ['it:%s' % left]

        if right in it_to_sc:
            right = to_to_sc[right]
        else:
            right = ['it:%s' % right]

        for l in left:
            for r in right:
                print("%s\t%s" % (l, r))
