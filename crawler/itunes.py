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

base_podcast_url = 'https://itunes.apple.com/WebObjects/MZStore.woa/wa/viewPodcast?mt=2&id=%s&cc=us'
base_info_url = 'https://client-api.itunes.apple.com/WebObjects/MZStorePlatform.woa/wa/lookup?id=%s'

def get(url):
    return AsyncHTTPClient().fetch(url, headers={
        'User-Agent':'iTunes/12.8.2 (Macintosh; OS X 10.11.6) AppleWebKit/601.7.8 (dt:1)',
        'x-apple-store-front':'143441-1,32',
        'x-apple-i-md-rinfo': '17106176',
        'x-apple-i-md-m':    'fZBobF3EL3WCeAW8rV70u70sVFTrpFbP7RuL/CDKbqT2uLuul0zMPT/1KU2zEZ57sUnmmfidystggy17',
        'origin': 'https://itunes.apple.com',
        'x-apple-i-md':  'AAAABQAAABAaQZ0PEUmWoMkDVlfcxS+fAAAAAQ==',
        'cookie':    'groupingPillToken=2_audioPodcasts; xp_ci=3z2oUPeTzDf7z4avzD6szusHqyZBi',
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
        proc.stdin.write(json.dumps(row))
        proc.stdin.write('\n')

def write_row(row):
    write_queue.put(row)

write_thread = threading.Thread(target=write_worker)
write_thread.daemon = True
write_thread.start()

@itunes.define('podcast')
def get_podcast(podcast_id=None):
    podcast_url = base_podcast_url % podcast_id
    response = yield get(podcast_url)
    print response.code
    if response.code != 200:
        return
    try:
        data = get_data_blob(response.body)
    except:
        data = False

    if data is not False:
        print data['pageData'].keys()
        write_row({'podcast_id':podcast_id, 'html_data':data})

        for _id in get_linking_ids(data):
            yield ('podcast', {'podcast_id':_id}, 'podcast:%s' % _id)

    #info_url = base_info_url % podcast_id
    #info_response = yield get(info_url)
    #print 'info', info_response.code
    #if info_response.code == 200:
    #    write_row({'podcast_id':podcast_id, 'info':json.loads(info_response.body)['results']})


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
