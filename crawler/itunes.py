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


base_podcast_url = 'https://itunes.apple.com/WebObjects/MZStore.woa/wa/viewPodcast?mt=2&id=%s'

base_info_url = r'https://client-api.itunes.apple.com/WebObjects/MZStorePlatform.woa/wa/lookup?id=%(ids)s&p=%(p)s&caller=%(caller)s&eersiotParameters=%%5Bobject%%20Object%%5D&=1&artwork=room_dt&X-JS-SP-TOKEN=%(token)s&X-JS-TIMESTAMP=%(timestamp)d'
def info_url(ids):
    ids_string = ','.join(ids)
    timestamp = int(time.time())
    storefront = '143441-1,32'
    caller = 'DI6'
    p = 'lockup'
    sig_string = str(timestamp) + storefront + caller + ids_string + p
    token = urllib.quote(sha1(sig_string).digest().encode('base64').strip()) + '%3D'
    return base_info_url % dict(
            timestamp=timestamp,
            caller=caller,
            p=p,
            token=token,
            ids=urllib.quote(ids_string))


def get(url):
    return AsyncHTTPClient().fetch(url, headers={
        'User-Agent':'iTunes/12.8.2 (Macintosh; OS X 10.11.6) AppleWebKit/601.7.8 (dt:1)',
        'x-apple-store-front':'143441-1,32',
        #'x-apple-i-md-rinfo': '17106176',
        #'x-apple-i-md-m':    'fZBobF3EL3WCeAW8rV70u70sVFTrpFbP7RuL/CDKbqT2uLuul0zMPT/1KU2zEZ57sUnmmfidystggy17',
        #'origin': 'https://itunes.apple.com',
        #'x-apple-i-md':  'AAAABQAAABAaQZ0PEUmWoMkDVlfcxS+fAAAAAQ==',
        #'cookie':    'groupingPillToken=2_audioPodcasts; xp_ci=3z2oUPeTzDf7z4avzD6szusHqyZBi',
        'referer':'https://itunes.apple.com/WebObjects/MZStore.woa/wa/viewGrouping?cc=us&id=33'
        },
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
    res.update(data['popularityMap']['podcastEpisode'].keys())
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
    response = yield get(podcast_url)
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

    #ids_batch.add(podcast_id)
    #if len(ids_batch) >= 15:
    #    url = info_url(list(ids_batch))
    #    response = yield get(url)
    #    print response
    #    if response.code == 200:
    #        write_row(response.body)
    #    ids_batch.clear()


crawl = crawler.Crawler(queue_db, handlers=itunes.get(), max_requests=100)

start_user_id = '258530615'
crawl.push_job('podcast', {'podcast_id':start_user_id}, 'podcast:%s' % start_user_id)


crawl.start_job()

import tornado.ioloop as ioloop

loop = ioloop.IOLoop.instance()

def shutdown():
    #loop.stop()
    #writer_queue.join()
    sys.exit()

loop.call_later(10*60, shutdown)

loop.start()
