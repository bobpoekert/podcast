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

user_agent = random.choice(http.user_agents)

def get(url):
    return AsyncHTTPClient().fetch(url, headers={
        'User-Agent':user_agent,
        'Origin':'https://soundcloud.com',
        'Referer':'https://soundcloud.com/'},
        request_timeout=2,
        raise_error=False)


base_followers_url = 'https://api-v2.soundcloud.com/users/%s/%s?client_id=ZDEMitjIfakbUYLF1Lmis76TTlKUi49C&limit=20&offset=0&linked_partitioning=1&app_version=1549888089&app_locale=en'
base_links_url = 'https://api-v2.soundcloud.com/users/soundcloud:users:%s/web-profiles?client_id=ZlszkiBH5xW8a8131srowVikrbbWTuug&app_version=1550743924&app_locale=en'

import subprocess


import sys
import threading
from Queue import Queue
queue_db = sys.argv[1]
output = sys.argv[2]
soundcloud = crawler.TaskSet()

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


def _get_data(attr, user_id):
    #yield sleep(random.random() * 0.5)
    print 'get', attr, user_id
    url = base_followers_url % (user_id, attr)
    print url
    idx = 0
    tries = 0
    pages = 0
    while url and tries < 5 and pages < 10:
        response = yield get(url)
        print 'status:', response.code
        if response.code != 200:
            break
            #tries += 1
            #yield sleep(random.random() * 10.0)
            continue
        try:
            obj = json.loads(response.body)
        except:
            print response
            #yield sleep(random.random() * 10.0)
            break
        followers = obj.get('collection')
        if followers:
            write_row({'user_id':user_id, attr:followers})
            idx += 1
            for user in followers:
                if user.get('kind') != 'user':
                    continue
                target_user_id = user['id']
                yield ('user', {'user_id':target_user_id}, 'user:%r' % target_user_id)
            url = obj['next_href']
            pages += 1
        else:
            break

@soundcloud.define('user')
def get_followers(user_id=None):
    return _get_data('followers', user_id)

@soundcloud.define('user')
def get_followings(user_id=None):
    return _get_data('followings', user_id)

@soundcloud.define('user')
def get_likes(user_id=None):
    return _get_data('likes', user_id)

@soundcloud.define('user')
def get_comments(user_id=None):
    return _get_data('comments', user_id)

@soundcloud.define('user')
def get_links(user_id=None):
    url = base_links_url % user_id
    response = yield get(url)
    write_row({'user_id':user_id, 'links':json.loads(response.body)})


crawl = crawler.Crawler(queue_db, handlers=soundcloud.get(), max_requests=10)

start_user_id = '33831735' # this american life
#start_user_id = '203413580' # joe rogan
crawl.push_job('user', {'user_id':start_user_id}, None)

crawl.start_job()

import tornado.ioloop as ioloop

loop = ioloop.IOLoop.instance()

def shutdown():
    #loop.stop()
    #writer_queue.join()
    sys.exit()

loop.call_later(10*60, shutdown)

loop.start()
