import crawler
from tornado.httpclient import AsyncHTTPClient
import lxml.etree as etree
import gzip
from io import BytesIO
import subprocess
import traceback

def gzip_decode(blob):
    sio = BytesIO(blob)
    gz = gzip.GzipFile(filename='', fileobj=sio, mode='r')
    return gz.read()

def get(url):
    return AsyncHTTPClient().fetch(url, request_timeout=10, raise_error=False)

def get_no_timeout(url):
    return AsyncHTTPClient().fetch(url, raise_error=False)

def meta_url(url):
    if '/details/' not in url:
        return None
    identifier = url.split('/details/')[1]
    return 'http://archive.org/download/%s/%s_meta.xml' % (identifier, identifier)

archive = crawler.TaskSet()

import sys
import threading
from queue import Queue
soundcloud = crawler.TaskSet()

output = None

write_queue = Queue(maxsize=5)

def write_worker():
    print(output)
    with open(output, 'ab+') as outf:
        proc = subprocess.Popen(['gzip', '-c'], stdin=subprocess.PIPE, stdout=outf)
        while 1:
            try:
                row = write_queue.get()
                if type(row) == str:
                    row = row.encode('utf-8`')
                proc.stdin.write(row.replace(b'\n', b' '))
                proc.stdin.write(b'\n')
            except:
                traceback.print_exc()

def write_row(row):
    write_queue.put(row)

def xpath(tree, q):
    return tree.xpath(q, namespaces={'s':'http://www.sitemaps.org/schemas/sitemap/0.9'})

def check_url(url):
    return 'WIDE-' not in url and 'fav-' not in url and 'top_domains-' not in url

@archive.define('sitemap')
def sitemap(url=None):
    assert url is not None
    print(url)
    response = yield get_no_timeout(url)
    print(response.code)
    body = response.body
    if url.endswith('.gz'):
        body = gzip_decode(body)
    tree = etree.XML(body)
    for sitemap in xpath(tree, '//s:sitemap/s:loc/text()'):
        yield ('sitemap', {'url':sitemap}, sitemap)
    for url in xpath(tree, '//s:urlset/s:url/s:loc/text()'):
        if check_url(url):
            yield ('meta', {'url':url}, url)

@archive.define('meta')
def page_url(url=None):
    if not check_url(url):
        return
    print(url)
    url = meta_url(url)
    if url is not None:
        response = yield get(url)
        if response.code == 200:
            write_row(response.body)
        else:
            print(response.code, response.body)

if __name__ == '__main__':
    queue_db = sys.argv[1]
    output = sys.argv[2]

    write_thread = threading.Thread(target=write_worker)
    write_thread.daemon = True
    write_thread.start()

    crawl = crawler.Crawler(queue_db, handlers=archive.get(), max_requests=50)
    crawl.push_job('sitemap', {'url':'https://archive.org/sitemap/sitemap.xml'}, 'https://archive.org/sitemap/sitemap.xml')
    crawl.start_job()

    import tornado.ioloop as ioloop

    loop = ioloop.IOLoop.instance()

    loop.start()
