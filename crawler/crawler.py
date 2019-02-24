import sqlite3
import traceback
try:
    import ujson as json
except ImportError:
    import json
import time
from tornado.ioloop import IOLoop
import random
import threading
from Queue import Queue

class Crawler(object):

    def __init__(self, queue_fname, handlers=({}, None), max_requests=100, ioloop=None):
        print 'max:', max_requests
        self.queue_fname = queue_fname
        self.queue_db = sqlite3.connect(queue_fname)
        self.message_handlers = handlers
        self.max_requests = max_requests
        self.running_requests = set([])
        self.ioloop = ioloop if ioloop is not None else IOLoop.instance()
        self.n_timeouts = 0
        self.n_requests = 0
        self.init_db()

        self.db_queue = Queue(maxsize=5)
        self.db_thread = threading.Thread(target=self.db_runner)
        self.db_thread.daemon = True
        self.db_thread.start()

    def add_req_id(self, _id):
        self.running_requests.add(_id)

    def del_req_id(self, _id):
        self.running_requests.remove(_id)

    def n_reqs(self):
        return len(self.running_requests)

    def db_run(self, kind, args):
        self.db_queue.put((kind, args))

    def db_runner(self):
        db = sqlite3.connect(self.queue_fname)
        while 1:
            kind, args = self.db_queue.get()
            if kind == 'add_url':
                url = args
                db.execute('insert or ignore into urls (url) values (?)', (url,))
                db.commit()
            elif kind == 'push_job':
                db.execute('insert into queue (type, payload, created_on, started, complete, url) values (?, ?, time(\'now\'), 0, 0, ?)', args)
                db.commit()
            elif kind == 'delete_job':
                db.execute('delete from queue where id = ?', (args,))
                db.commit()

    def add_handler(self, k, v):
        assert k not in self.message_handlers
        self.message_handlers[k] = v

    def init_db(self):
        with self.queue_db:
            cur = self.queue_db.cursor()
            cur.execute('create table if not exists queue (id integer primary key, payload text, type text, created_on integer, started boolean, complete boolean, url text)')
            cur.execute('create index if not exists popper on queue (id)')
            cur.execute('create index if not exists popper_exists on queue (started)')
            cur.execute('create table if not exists urls (url text, constraint url_unique unique (url))')
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
        self.db_run('add_url', url)

    def push_job(self, typ, payload, url):
        if url is not None and self.contains_url(url):
            return
        if url is not None:
            self.add_url(url)
        payload_blob = json.dumps(payload)
        self.db_run('push_job', (typ, payload_blob, url))

    def pop_job(self):
        with self.queue_db:
            while 1:
                cur = self.queue_db.cursor()
                cur.execute(
                        'select id, type, payload, url from queue where started != 1 and id not in (%s) order by id limit 1' % \
                                ','.join(map(str, self.running_requests)))
                row = cur.fetchone()
                if not row:
                    return None
                _id, _type, payload_data, url = row
                if _id in self.running_requests:
                    continue
                print _type
                #cur.execute('update queue set started = 1 where id = ?', (_id,))
                #self.queue_db.commit()
                return (_id, _type, json.loads(payload_data), url)

    def finish_job(self, _id):
        self.job_finished(_id)
        print 'running:', self.running_requests
        if _id is not None:
            self.db_run('delete_job', _id)

    def start_job(self):
        print self.running_requests, self.max_requests
        if self.n_reqs() < self.max_requests:
            print 'pop'
            job = self.pop_job()
            print job
            if job:
                self.execute(*job)

    def execute(self, _id, _type, payload, url):
        self.running_requests.add(_id)
        handler = self.message_handlers[_type]
        handler(_id, payload, self, url)
        self.n_requests += 1

    def timeout(self):
        self.n_timeouts += 1

    def delay_interval(self):
        return random.randint(2**(self.n_timeouts / self.n_requests) - 1)

    def job_finished(self, _id):
        print 'finished'
        self.del_req_id(_id)
        #self.ioloop.add_callback(self.start_job)
        self.start_job()

class Heartbeat(object):

    def __init__(self, duration):
        self.duration = duration

class Task(object):

    def __init__(self, _id, payload, crawler, generator, url, ioloop=None):
        crawler.add_req_id(_id)
        self.id = _id
        self.url = url
        self.payload = payload
        self.crawler = crawler
        self.generator = generator(**payload)
        self.state = None
        self.is_done = False
        self.current_future = None
        if ioloop is None:
            ioloop = IOLoop.instance()
        self.ioloop = ioloop
        self.pump()

    def pump(self):
        self.crawler.add_req_id(self.id)
        if self.is_done:
            return
        loop = False
        try:
            nxt = self.generator.send(self.state)
            self.current_future = None
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
            else: # future
                nxt.add_done_callback(self.on_future)
                self.current_future = nxt
        except StopIteration:
            return
        except Exception, e:
            print 'error in %s' % self.url
            traceback.print_exc()
            return
        finally:
            if loop:
                self.ioloop.add_callback(self.pump)

    def on_future(self, result):
        self.state = result.result()
        self.pump()

    def __del__(self):
        self.done()

    def done(self):
        if not self.is_done:
            self.is_done = True
            #self.ioloop.remove_timeout(self.timeout)
            self.crawler.finish_job(self.id)
            if self.current_future is not None:
                self.current_future.cancel()

class TaskSet(object):

    def __init__(self):
        self.handlers = {}

    def define(self, keyname):
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
        return self.handlers

    def merge(self, other_task_set):
        res = TaskSet()
        res.handlers = dict(self.handlers, **other_task_set.handlers)
        return res

    def copy(self):
        res = TaskSet()
        res.handlers = self.handlers.copy()
        return res
