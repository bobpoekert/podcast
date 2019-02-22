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

user_agent = random.choice(http.user_agents)

def get(url):
    return AsyncHTTPClient().fetch(url, headers={
        'User-Agent':user_agent,
        'Origin':'https://soundcloud.com',
        'Referer':'https://soundcloud.com/'},
        raise_error=False)


base_url = 'https://api-v2.soundcloud.com/users/%s/%s?client_id=ZDEMitjIfakbUYLF1Lmis76TTlKUi49C&limit=100&offset=0&linked_partitioning=1&app_version=1549888089&app_locale=en'


soundcloud = crawler.TaskSet()

def _get_follows(attr, user_id):
    url = base_url % (user_id, attr)
    idx = 0
    while url:
        response = yield get(url)
        try:
            obj = json.loads(response.body)
        except ValueError:
            break
        followers = obj.get('collection')
        print len(followers)
        if followers:
            yield (attr, {'user_id':user_id, attr:followers}, '%s:%s:%d' % (attr, user_id, idx))
            idx += 1
            for user in followers:
                target_user_id = user['id']
                yield ('user', {'user_id':target_user_id}, 'user:%r' % target_user_id)
            url = obj['next_href']
        else:
            break

@soundcloud.define('user')
def get_followers(user_id=None):
    return _get_follows('followers', user_id)

@soundcloud.define('user')
def get_followings(user_id=None):
    return _get_follows('followings', user_id)

import subprocess


if __name__ == '__main__':
    import sys
    queue_db = sys.argv[1]
    output = sys.argv[2]
    crawl = crawler.Crawler(queue_db, handlers=soundcloud.get(), max_requests=10)

    outf = open(output, 'a+')
    proc = subprocess.Popen(['xz', '-z', '-c'], stdout=outf, stdin=subprocess.PIPE)

    def write_row(_id, row, crawler, url):
        proc.stdin.write(json.dumps(row))
        proc.stdin.write('\n')

    crawl.add_handler('followers', write_row, True)
    crawl.add_handler('followings', write_row, True)


    start_user_id = '33831735' # this american life
    #start_user_id = '203413580' # joe rogan
    #crawl.push_job('user', {'user_id':start_user_id}, None)

    crawl.start_job()

    import tornado.ioloop as ioloop

    def shutdown():
        sys.exit()

    ioloop.IOLoop.instance().call_later(60*60, shutdown)

    ioloop.IOLoop.instance().start()
