import lxml.etree as etree
import lxml.html as html
import gzip
from urllib.parse import urljoin, urlparse
from warcio.archiveiterator import ArchiveIterator
from nltk.tokenize import word_tokenize
import re

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
def text(tree):
    res = []
    for path in rss_paths:
        for row in xpath(tree, path):
            res.extend(html_text(row))
    res.extend(xpath(tree, '//itunes:category/@text'))
    return res

def rss_text(blob):
    return text(etree.fromstring(blob))

def episodes(tree):
   return xpath(tree, '//item')

def guid(url, episode):
    try:
        v = episode.xpath('.//guid/text()')[0]
    except IndexError:
        try:
            v = episode.xpath('.//link/text()')[0]
        except IndexError:
            v = episode.xpath('.//enclosure/@url')
    domain = urlparse(url).netloc
    return '%s@%s' % (v, domain)
        

def tokens(texts):
    res = []
    for t in texts:
        res.extend(word_tokenize(t))
    return [v.lower() for v in res]

def warc_pages(fname):
    with gzip.open(fname, 'r') as inf:
        req = None
        for record in ArchiveIterator(inf):
            if record.rec_type == 'request':
                req = record
            elif record.rec_type == 'response':
                yield (req, record)
