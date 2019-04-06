import pickle
import sys

res = []
for fname in sys.argv[2:]:
    with open(fname, 'rb') as inf:
        data = pickle.load(inf)
        for idx, batch in enumerate(data):
            if len(res) <= idx:
                for i in range(len(res), idx+1):
                    res.append({})
            print(idx, len(res))
            target = res[idx]
            for k, c in batch:
                if k in target:
                    target[k] += c
                else:
                    target[k] = c

with open(sys.argv[1], 'wb') as outf:
    pickle.dump(res, outf)
