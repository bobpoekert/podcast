#!/usr/bin/env python3
import lxml.etree as etree
import lxml.html as html
import numpy as np
import gzip
from urllib.parse import urljoin
from warcio.archiveiterator import ArchiveIterator
from collections import Counter
import traceback
import os
from multiprocessing import cpu_count, Queue
from nltk.tokenize import word_tokenize
import soundcloud_mat

def html_text(blob):
    try:
        tree = html.fromstring(blob)
        return list(tree.itertext())
    except:
        return [blob]

namespaces = {
    "atom":"http://www.w3.org/2005/Atom",
    "cc":"http://web.resource.org/cc/",
    "itunes":"http://www.itunes.com/dtds/podcast-1.0.dtd",
    "media":"http://search.yahoo.com/mrss/",
    "content":"http://purl.org/rss/1.0/modules/content/",
    "rdf":"http://www.w3.org/1999/02/22-rdf-syntax-ns#"}

def xpath(tree, q):
    return tree.xpath(q, namespaces=namespaces)

rss_tags = ('title', 'description', 'itunes:summary', 'itunes:keywords', 'itunes:subtitle')
rss_paths = ['//%s/text()' % v for v in rss_tags]
def rss_text(blob):
    tree = etree.fromstring(blob)
    res = []
    for path in rss_paths:
        for row in xpath(tree, path):
            res.extend(html_text(row))
    res.extend(xpath(tree, '//itunes:category/@text'))
    return res

def tokens(texts):
    res = []
    for t in texts:
        res.extend(word_tokenize(t))
    return res


feed_url_to_pb = {}
with open('/mnt/tiny/podbean/feed_urls.txt', 'r') as inf:
    for row in inf:
        try:
            _id, url = row.strip().split()
        except:
            print(row)
            continue
        feed_url_to_pb[url] = _id

centers = np.fromfile('cluster_centers_128_4096.npy').reshape((-1, 128))
fact = soundcloud_mat.LSAFactorizer('/mnt/lappy2/soundcloud/', '/mnt/lappy2/combined_graph_pb.tsv.gz')
vec_128 = fact.squished

def l1_distances(mat, vec):
    assert mat.shape[1] == vec.shape[0]
    return np.sum(np.abs(mat - vec), axis=1) / vec.shape[0]

def get_cluster(sc_id):
    vec = vec_128[fact.id_dict[sc_id.encode('utf-8')]]
    return np.argmin(l1_distances(centers, vec))

itunes_to_soundcloud = {}
with open('itunes_soundcloud_ids.txt', 'r') as inf:
    for row in inf:
        sc, it = row.strip().split()
        itunes_to_soundcloud[it] = sc

podbean_to_soundcloud = {}
with open('/mnt/tiny/podbean/soundcloud_ids.txt', 'r') as inf:
    for row in inf:
        podbean, soundcloud = row.strip().split()
        podbean_to_soundcloud[podbean] = soundcloud

podbean_to_itunes = {}
with open('/mnt/tiny/podbean/itunes_url_join.txt', 'r') as inf:
    for row in inf:
        url, itunes, podbean = row.strip().split()
        podbean_to_itunes[podbean] = itunes

def id_from_pb(pb):
    if pb in podbean_to_soundcloud:
        return podbean_to_soundcloud[pb]
    if pb in podbean_to_itunes:
        it = podbean_to_itunes[pb]
        if it in itunes_to_soundcloud:
            return itunes_to_soundcloud[it]
        else:
            return 'it:%s' % it
    else:
        return pb

url_to_pb = {}
with gzip.open('/mnt/zapk/podbean_graph.tsv.gz', 'rb') as inf:
    for row in inf:
        l, r, url = row.strip().split(b'\t')
        url_to_pb[url.decode('utf-8')] = r

def id_from_url(url):
    if 'soundcloud:user' in url:
        return re.search(r'soundcloud:user:(\d+)', url).group(1)
    else:
        try:
            pb = feed_url_to_pb[url]
        except KeyError:
            pb = url_to_pb[url]
        return id_from_pb(pb)

def process_page(dists, req, rsp):
    url = req.rec_headers.get_header('WARC-Target-URI')
    _id = id_from_url(url)
    cluster = get_cluster(_id)
    page = rsp.content_stream().read()
    tk = tokens(rss_text(page))
    dists[cluster].update(tk)

def process_file(dists, fname):
    with gzip.open(fname, 'r') as inf:
        req = None
        for record in ArchiveIterator(inf):
            if record.rec_type == 'request':
                req = record
            elif record.rec_type == 'response':
                try:
                    process_page(dists, req, record)
                except:
                    traceback.print_exc()

def worker(outfname, infname_queue, outp_queue):
    dists = [Counter() for i in range(centers.shape[0])]
    while 1:
        fname = infname_queue.get()
        print(fname)
        if fname is False:
            break
        process_file(dists, fname)

    with open(outfname, 'wb') as outf:
        pickle.dump([list(d.items()) for d in dists], outf)

def merge_batch(a, b):
    for k, v in b.items():
        try:
            a[k] += v
        except KeyEror:
            a[k] = v

if __name__ == '__main__':
    import sys, pickle
    outfname = sys.argv[1]
    inputs = sys.argv[2:]
    inq = Queue()
    for inp in inputs:
        inq.put(inp)
    for i in range(cpu_count()):
        inq.put(False)
    outq = Queue()
    for i in range(cpu_count()):
        if os.fork() == 0:
            worker('%s.%s' % (outfname, i), inq, outq)
            sys.exit()
