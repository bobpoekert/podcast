from tornado.httpclient import AsyncHTTPClient, HTTPResponse, HTTPRequest
import urlparse, random
try:
    from concurrent.futures import Future
except ImportError:
    from tornado.concurrent import Future
from tornado.concurrent import chain_future
from tornado.ioloop import IOLoop
from functools import partial
from datetime import timedelta

AsyncHTTPClient.configure("tornado.curl_httpclient.CurlAsyncHTTPClient")

request_count = 0
request_time = 0

user_agents = [row.strip() for row in open('user_agents.txt', 'r')]

proxies = []
with open('proxies.txt', 'r') as f:
    for row in f:
        try:
            h, p = row.strip().split(':')
            proxies.append((h, int(p)))
        except:
            pass

random.shuffle(proxies)
proxy_successes = {}
proxy_failures = {}

def proxy_score(proxy):
    successes = proxy_successes.get(proxy, 0)
    failures = proxy_failures.get(proxy, 0)
    return successes**2/(failures + 1)

def sort_proxies():
    proxies.sort(key=proxy_score, reverse=True)

def bounded_pareto(low, high):
    delta = high - low
    p = min(random.paretovariate(2), 5) / 5.0
    return low + p * delta

def get_proxy():
    sort_proxies()
    idx = int(bounded_pareto(0, len(proxies)-1))
    return proxies[idx]

def proxy_success(proxy):
    if proxy is None:
        return
    try:
        proxy_successes[proxy] += 1
    except KeyError:
        proxy_successes[proxy] = 1

def proxy_fail(proxy):
    if proxy is None:
        return
    try:
        proxy_failures[proxy] += 1
    except KeyError:
        proxy_failures[proxy] = 1
    successes = proxy_successes.get(proxy, 0)
    failures = proxy_failures.get(proxy, 0)
    # print failures, proxy

request_timeout = 1

class on_inner_response(object):

    def __init__(self, inner_future):
        self.inner_future = inner_future
        self.done = False

    def __call__(self, value):
        if self.done:
            #if value is not None:
                #print 'too late'
            return
        self.done = True
        if value is None:
            self.inner_future.set_result(value)
        else:
            self.inner_future.set_result(value.result())


def raw_get(url):
   return AsyncHTTPClient().fetch(HTTPRequest(url, headers={'User-Agent':random.choice(user_agents)}), raise_error=False)

def get(base_url, url, retries=0, ioloop=None, use_proxies=False, user_agent=None, streaming_callback=None):
    if ioloop is None:
        ioloop = IOLoop.instance()
    if user_agent is None:
        user_agent = random.choice(user_agents)
    client = AsyncHTTPClient()
    if base_url:
        url = urlparse.urljoin(base_url, url)
    if retries > 10:
       #print 'giving up on %s' % url
       return
    print retries, url
    if use_proxies:
        proxy = random.choice(proxies)
        proxy_host, proxy_port = proxy
        request = HTTPRequest(
                url,
                headers={'User-Agent':user_agent},
                connect_timeout=0.5,
                request_timeout=request_timeout,
                proxy_host=proxy_host,
                proxy_port=proxy_port)
    else:
        proxy = None
        request = HTTPRequest(url, headers={'User-Agent':user_agent}, request_timeout=request_timeout)
    resp = client.fetch(request, raise_error=False, streaming_callback=streaming_callback)
    inner_future = Future()
    result_future = Future()

    handler = on_inner_response(inner_future)

    resp.add_done_callback(handler)
    # This is a hack to work around a possible bug in tornado and/or pycurl where invalid HTTP responses don't get parsed.
    # Since we set a request_timeout in the HTTPRequest we know that if it's been longer than that and the future 
    # hasn't been realized then it will never be realized and we should call the response callback ourselves.
    ioloop.add_timeout(timedelta(seconds=20), partial(handler, None))

    inner_future.add_done_callback(partial(on_response, result_future, url, retries, proxy))

    return result_future

def on_response(future, url, retries, proxy, response_future):
    global request_count
    global request_time
    response = response_future.result()
    print response

    def retry():
        new_req = get(url, url, retries+1, use_proxies=proxy is not None)
        if new_req:
            chain_future(new_req, future)
        else:
            future.set_result(response)

    if response is None: # timeout called instead of pycurl callback
        proxy_fail(proxy)
        retry()
        request_time += request_timeout
        return

    request_time += response.time_info['total']
    if response.code > 299 and retries < 10:
        retry()
        proxy_fail(proxy)
    else:
        request_count += 1
        print 'avg request time: %f' % (request_time / float(request_count))
        future.set_result(response)
        if response.code <= 299:
            proxy_success(proxy)

