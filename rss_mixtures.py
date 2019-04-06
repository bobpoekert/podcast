import rss
import mixture
import sys
import pickle
import lxml.etree as etree
from io import StringIO
import numpy as np
import traceback

with open('/mnt/lappy/word_dists_clean.pickle', 'rb') as inf:
    topics = mixture.TopicModel(pickle.load(inf))

def to_csv(arr):
    return ','.join(map(str, arr))
    res = StringIO()
    np.savetxt(res, arr.flatten(), delimiter=',')
    return res.getvalue().strip()

def dist(vs, res):
    for v in vs:
        if v in res:
            res[v] += 1
        else:
            res[v] = 1
    return res

if __name__ == '__main__':
    infiles = sys.argv[1:]
    for inf in infiles:
        for page in rss.warc_pages(inf):
            try:
                url = page[0].rec_headers.get_header('WARC-Target-URI')
                tree = etree.fromstring(page[1].content_stream().read())
            except:
                continue
            d = dist(rss.tokens(rss.text(tree)), {})
            for episode in rss.episodes(tree):
                dd = dist(rss.tokens(rss.text(episode)), d.copy())
                vec = topics.topic_proportions(topics.dist_vector(dd))
                guid = rss.guid(url,episode)
                print('%s\t%s\t%s' % (url, guid, to_csv(vec)))
