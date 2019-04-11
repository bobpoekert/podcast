import crawler
from tornado.httpclient import AsyncHTTPClient
import random
import http_ as http
import re, urlparse
from urllib import quote
try:
    import ujson as json
except ImportError:
    import json
from itertools import izip
from tornado.gen import sleep
import subprocess
from hashlib import sha1
from base64 import b64encode
import time
import urllib
from tornado.websocket import WebSocketHandler
from tornado.concurrent import Future
from tornado.web import Application, StaticFileHandler, RequestHandler
import tornado.gen as gen
from tornado.httpserver import HTTPServer
from os.path import abspath
import traceback


AsyncHTTPClient.configure("tornado.curl_httpclient.CurlAsyncHTTPClient", max_clients=100)

itunes_ip = '104.69.19.120'

base_podcast_url = 'https://' + itunes_ip + '/WebObjects/MZStore.woa/wa/viewPodcast?id=%s'
base_podcast_host = 'itunes.apple.com'

base_info_url = 'https://' + itunes_ip + '/WebObjects/MZStorePlatform.woa/wa/lookup?id=%(ids)s&p=item&caller=DI6&requestParameters=%%5Bobject%%20Object%%5D&version=1&X-JS-SP-TOKEN=%(token)s&X-JS-TIMESTAMP=%(timestamp)d'
base_info_host = 'client-api.itunes.apple.com'

def sig_string(ids, timestamp):
    timestamp = str(timestamp)
    caller = 'DI6'
    p = 'item'
    storefront = '143441-1,32'

    return timestamp + storefront + caller + ids + p

socket = None
sign_queue = []
class ItunesSignHandler(WebSocketHandler):

    def open(self):
        print 'connect'
        global socket
        global sign_queue
        socket = self
        self.job_id = 0
        self.jobs = {}
        for arg, job in sign_queue:
            self.sign(arg, f=job)
        sign_queue = []

    def on_close(self):
        print 'closed'
        global socket
        if socket == self:
            socket = None

    def new_job_id(self):
        res = self.job_id
        self.job_id += 1
        return res

    def sign(self, string, f=None):
        job_id = self.new_job_id()
        if f is None:
            job = Future()
        else:
            job = f
        self.jobs[job_id] = job
        self.write_message('%d %s' % (job_id, string))
        return job

    def on_message(self, message):
        print 'response'
        print message
        try:
            job_id, sig = message.split(' ', 1)
            job_id = int(job_id)
            job = self.jobs[job_id]
            del self.jobs[job_id]
            job.set_result(sig)
        except:
            traceback.print_exc()
            sys.exit()

    def check_origin(self, origin):
        return True

with open('itunes_sign.html', 'r') as inf:
    itunes_html = inf.read()

class SignWebpageHandler(RequestHandler):

    def get(self):
        self.write(itunes_html)

def sign(target):
    if socket is None:
        res = Future()
        sign_queue.append((target, res))
        return res
    else:
        return socket.sign(target)

def run_sig_server():
    app = Application([
        (r'/signer', ItunesSignHandler),
        (r'.*', SignWebpageHandler)
    ])
    server = HTTPServer(app, ssl_options=dict(certfile=abspath('./server.cert'), keyfile=abspath('./server.key')))
    server.listen(443)

@gen.coroutine
def info_url(ids):
    ids_string = ','.join(ids)
    timestamp = int(time.time())
    sig = yield sign(sig_string(ids_string, timestamp))
    raise gen.Return(base_info_url % dict(
        ids='%2C'.join(ids),
        token=quote(sig),
        timestamp=timestamp))

def get(url, host):
    return AsyncHTTPClient().fetch(url, headers={
        'Host': host,
        'User-Agent':'iTunes/12.8.2 (Macintosh; OS X 10.11.6) AppleWebKit/601.7.8 (dt:1)',
        'x-apple-store-front':'143441-1,32',
        'referer':'https://itunes.apple.com/WebObjects/MZStore.woa/wa/viewGrouping?cc=us&id=33'
        },
        validate_cert=False,
        request_timeout=2,
        raise_error=False)

def get_data_blob(page):
    blob = page.split('its.serverData=', 1)[1].split('</script>', 1)[0]
    return json.loads(blob.strip())

def get_linking_ids(page_data):
    data = page_data['pageData']['podcastPageData']
    res = set([])
    res.update(data['listenersAlsoBought'])
    res.update(data['topPodcastsInGenre'])
    #res.update(data['popularityMap']['podcastEpisode'].keys())
    return res

import sys
import threading
from Queue import Queue
queue_db = sys.argv[1]
output = sys.argv[2]

itunes = crawler.TaskSet()

outf = open(output, 'a+')
proc = subprocess.Popen(['xz', '-z', '-c'], stdout=outf, stdin=subprocess.PIPE)

write_queue = Queue(maxsize=5)

def write_worker():
    while 1:
        row = write_queue.get()
        proc.stdin.write(row)
        proc.stdin.write('\n')

def write_row(row):
    write_queue.put(row)

write_thread = threading.Thread(target=write_worker)
write_thread.daemon = True
write_thread.start()

ids_batch = set([])

@itunes.define('podcast')
def get_podcast(podcast_id=None):
    podcast_url = base_podcast_url % podcast_id
    response = yield get(podcast_url, base_podcast_host)
    print response.code
    if response.code != 200:
        return
    if '<key>dialogId</key><string>itemNotAvailable</string>' not in response.body:
        data = get_data_blob(response.body)
    else:
        data = False

    if data is not False:
        print data['pageData'].keys()
        write_row(json.dumps({'podcast_id':podcast_id, 'html_data':data}))

        for _id in get_linking_ids(data):
            yield ('podcast', {'podcast_id':_id}, 'podcast:%s' % _id)

    ids_batch.add(podcast_id)
    if len(ids_batch) >= 15:
        ids = list(ids_batch)
        ids_batch.clear()
        url = yield info_url(ids)
        response = yield get(url, base_info_host)
        #print response
        if response.code == 200:
            print 'info'
            write_row(response.body)


crawl = crawler.Crawler(queue_db, handlers=itunes.get(), max_requests=100)

#start_user_id = '258530615'
start_user_id = '360084272' # joe rogan
crawl.push_job('podcast', {'podcast_id':start_user_id}, 'podcast:%s' % start_user_id)

start_user_id2 = '1342684917' # c&en
crawl.push_job('podcast', {'podcast_id':start_user_id2}, 'podcast:%s' % start_user_id2)


crawl.start_job()

import tornado.ioloop as ioloop

loop = ioloop.IOLoop.instance()

def shutdown():
    #loop.stop()
    #writer_queue.join()
    sys.exit()

loop.call_later(10*60, shutdown)

run_sig_server()

loop.start()
