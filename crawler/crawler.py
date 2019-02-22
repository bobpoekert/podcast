import sqlite3
import traceback
try:
    import ujson as json
except ImportError:
    import json
import time
from tornado.ioloop import IOLoop

class Crawler(object):

    def __init__(self, queue_fname, handlers=({}, None), max_requests=100, ioloop=None):
        handlers, immediate = handlers
        print 'max:', max_requests
        self.queue_db = sqlite3.connect(queue_fname)
        self.message_handlers = handlers
        self.immediate = immediate if immediate is not None else set([])
        self.max_requests = max_requests
        self.running_requests = 0
        self.ioloop = ioloop if ioloop is not None else IOLoop.instance()
        self.init_db()

    def add_handler(self, k, v, is_immediate):
        assert k not in self.message_handlers
        self.message_handlers[k] = v
        if is_immediate:
            self.immediate.add(k)

    def init_db(self):
        with self.queue_db:
            cur = self.queue_db.cursor()
            cur.execute('create table if not exists queue (id integer primary key, payload text, type text, created_on integer, started boolean, complete boolean, url text)')
            cur.execute('create index if not exists popper on queue (id)')
            cur.execute('create index if not exists popper_exists on queue (started)')
            cur.execute('create table if not exists urls (url text)')
            cur.execute('create unique index if not exists url_exists on urls (url)')

    def contains_url(self, url):
        if url is None:
            return False
        cur = self.queue_db.cursor()
        cur.execute('select url from urls where url = ?', (url,))
        res = cur.fetchone()
        return bool(res)

    def add_url(self, url):
        if url is None:
            return
        with self.queue_db:
            cur = self.queue_db.cursor()
            cur.execute('insert or ignore into urls (url) values (?)', (url,))

    def push_job(self, typ, payload, url):
        if url is not None and self.contains_url(url):
            return
        if url is not None:
            self.add_url(url)
        if typ in self.immediate:
            print 'immediate', typ
            self.execute(None, typ, payload, url)
        else:
            payload_blob = json.dumps(payload)
            with self.queue_db:
                cur = self.queue_db.cursor()
                cur.execute('insert into queue (type, payload, created_on, started, complete, url) values (?, ?, time(\'now\'), 0, 0, ?)', (typ, payload_blob, url))

    def pop_job(self):
        with self.queue_db:
            cur = self.queue_db.cursor()
            cur.execute('select id, type, payload, url from queue where started != 1 order by id limit 1')
            row = cur.fetchone()
            if not row:
                return None
            _id, _type, payload_data, url = row
            print _type
            cur.execute('update queue set started = 1 where id = ?', (_id,))
            return (_id, _type, json.loads(payload_data), url)

    def finish_job(self, _id):
        self.job_finished()
        print 'running:', self.running_requests
        if _id is not None:
            with self.queue_db:
                cur = self.queue_db.cursor()
                #cur.execute('update queue set complete = 1 where id = ?', (_id,))
                cur.execute('delete from queue where id = ?', (_id,))

    def start_job(self):
        print self.running_requests, self.max_requests
        while self.running_requests < self.max_requests:
            print 'pop'
            job = self.pop_job()
            print job
            if job:
                self.execute(*job)
                #self.ioloop.add_callback(self.start_job)
            else:
                break

    def execute(self, _id, _type, payload, url):
        handler = self.message_handlers[_type]
        handler(_id, payload, self, url)
        self.running_requests = min(self.running_requests+1, self.max_requests)

    def job_finished(self):
        print 'finished'
        self.running_requests -= 1
        if self.running_requests < 0:
            self.running_requests = 0
        self.ioloop.add_callback(self.start_job)
        #self.start_job()

class Heartbeat(object):

    def __init__(self, duration):
        self.duration = duration

class Task(object):

    def __init__(self, _id, payload, crawler, generator, url, ioloop=None):
        self.id = _id
        self.url = url
        self.payload = payload
        self.crawler = crawler
        self.generator = generator(**payload)
        self.state = None
        self.is_done = False
        if ioloop is None:
            ioloop = IOLoop.instance()
        self.timeout = ioloop.call_later(40, self.done)
        self.ioloop = ioloop
        self.pump()

    def pump(self):
        if self.is_done:
            return
        loop = False
        try:
            nxt = self.generator.send(self.state)
            if isinstance(nxt, tuple):
                if len(nxt) == 3:
                    typ, payload, url = nxt
                else:
                    typ, payload = nxt
                    url = None
                if not self.crawler.contains_url(url):
                    self.crawler.push_job(typ, payload, url)
                self.state = None
                loop = True
            elif isinstance(nxt, Heartbeat):
                self.ioloop.remove_timeout(self.timeout)
                self.timeout = self.ioloop.call_later(nxt.duration, self.done)
                loop = True
            else: # future
                nxt.add_done_callback(self.on_future)
        except StopIteration:
            self.done()
        except Exception, e:
            print 'error in %s' % self.url
            traceback.print_exc()
            self.done()
        finally:
            if loop:
                self.ioloop.add_callback(self.pump)

    def on_future(self, result):
        self.state = result.result()
        self.pump()

    def done(self):
        if not self.is_done:
            self.is_done = True
            self.ioloop.remove_timeout(self.timeout)
            self.crawler.finish_job(self.id)

class TaskSet(object):

    def __init__(self):
        self.immediate = set([])
        self.handlers = {}

    def define(self, keyname, is_immediate=False):
        if is_immediate:
            self.immediate.add(keyname)
        def inner_define(generator):
            if keyname in self.handlers:
                old_handler = self.handlers[keyname]
                def task_factory(_id, payload, crawler, url):
                    old_handler(_id, payload, crawler, url)
                    Task(_id, payload, crawler, generator, url)
            else:
                def task_factory(_id, payload, crawler, url):
                    Task(_id, payload, crawler, generator, url)
            self.handlers[keyname] = task_factory
            return task_factory
        return inner_define

    def get(self):
        return (self.handlers, self.immediate)

    def merge(self, other_task_set):
        res = TaskSet()
        res.immediate = self.immediate.union(other_task_set.immediate)
        res.handlers = dict(self.handlers, **other_task_set.handlers)
        return res

    def copy(self):
        res = TaskSet()
        res.immediate = self.immediate.copy()
        res.handlers = self.handlers.copy()
        return res
