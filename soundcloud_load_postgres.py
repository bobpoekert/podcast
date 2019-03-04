#!/usr/bin/python3
import sys, os
import subprocess as sp
import ujson as json
from multiprocessing import cpu_count, Queue, Pool
from threading import Thread
import tempfile
import psycopg2
import datetime
from math import ceil
import traceback

db = psycopg2.connect('')

inp_dir = sys.argv[1]
inp_fnames = [os.path.join(inp_dir, v) for v in os.listdir(inp_dir) if v.endswith('.jsons.xz')]

n_workers = cpu_count() / 2
fname_per_worker = ceil(len(inp_fnames) / n_workers)
fname_batches = [inp_fnames[v:(v+fname_per_worker)] for v in range(0, len(inp_fnames), fname_per_worker)]

user_cols_string = '''
        id integer, permalink text,  last_name text, country_code varchar(255), followings_count integer,\
        full_name text, groups_count integer, city text, first_name text, verified boolean, track_count integer,\
        playlist_count integer, username text, description text,\
        last_modified timestamp, created_at timestamp, uri text, avatar_url text, comments_count integer,\
        playlist_likes_count integer, urn text, is_pro boolean'''

user_cols = [v.strip().split() for v in user_cols_string.split(',')]
user_col_names = [v[0] for v in user_cols]

print(user_col_names)

track_cols_string = '''

    id integer, reposts_count integer, contains_music boolean, urn text, artist text, display_date timestamp,
    label_name text, duration integer, streamable boolean, user_id integer, title text, commentable boolean,
    state text, download_url text, comment_count integer, downloadable boolean, policy text, waveform_url text,
    has_downloads_left boolean, public boolean, sharing text, description text, secret_token text, purchase_url text,
    kind text, purchase_title text, last_modified timestamp, genre text, download_count integer, full_duration integer,
    permalink_url text, likes_count integer, permalink text, playback_count integer, license text, monetization_model text,
    artwork_url text, created_at timestamp, uri text, tag_list text, embeddable_by text

'''

track_cols = [v.strip().split() for v in track_cols_string.split(',')]
track_col_names = [v[0] for v in track_cols]
track_timestamp_cols = [v[0] for v in track_cols if v[1] == 'timestamp']
track_boolean_cols = [v[0] for v in track_cols if v[1] == 'boolean']

print(track_col_names)

cur = db.cursor()
cur.execute('create unlogged table if not exists soundcloud_links (user_id integer, url text, username text, network text, title text)')
cur.execute('create unlogged table if not exists soundcloud_users (%s)' % user_cols_string)
cur.execute('create unlogged table if not exists soundcloud_tracks (%s)' % track_cols_string)
cur.execute('create unlogged table if not exists soundcloud_following (from_id integer, to_id integer)')
cur.execute('create unlogged table if not exists soundcloud_likes (user_id integer, track_id integer, created_at timestamp, kind text)')
db.commit()

links_r, links_w = os.pipe()
users_r, users_w = os.pipe()
following_r, following_w = os.pipe()
tracks_r, tracks_w = os.pipe()
likes_r, likes_w = os.pipe()


def load_table(inp_pipe, table_name):
    uniq = sp.Popen(['hashuniq'], stdin=inp_pipe, stdout=sp.PIPE)
    copy = sp.Popen(['psql', '-c', 'copy %s from stdin delimiter \'\t\' null \'___\' encoding \'utf-8\'' % table_name], stdin=uniq.stdout)
    return copy

load_table(links_r, 'soundcloud_links')
load_table(users_r, 'soundcloud_users')
load_table(following_r, 'soundcloud_following')
load_table(tracks_r, 'soundcloud_tracks')
load_table(likes_r, 'soundcloud_likes')

row_queue = Queue(100)

def read_queue():
    worker_count = n_workers
    ctr = 0
    while worker_count > 0:
        ctr += 1
        row = row_queue.get()
        if ctr % 10000 == 0:
            print(ctr, row)
        if row == 'exit':
            worker_count -= 1
            continue
        table_name, vals = row
        if table_name == 'links':
            pipe = links_w
        elif table_name == 'users':
            pipe = users_w
        elif table_name == 'following':
            pipe = following_w
        elif table_name == 'tracks':
            pipe = tracks_w
        elif table_name == 'likes':
            pipe = likes_w
        else:
            print(table_name)
            continue
        os.write(pipe, ('%s\n' % vals).encode('utf-8'))

def expand_unicode(s):
    res = []
    for c in s:
        if c in 'qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890_-:. ':
            res.append(c)
        else:
            res.append('\\u%04x' % ord(c))
    return ''.join(res)

def serialize_cols(cols):
    res = ['true' if v is True else 'false' if v is False else r'___' if v is None else r'___' if v == '' else str(v) for v in cols]
    return '\t'.join(expand_unicode(v) for v in res)

def translate_timestamp(ts_string):
    if ts_string:
        ts = datetime.datetime.strptime(ts_string, "%Y-%m-%dT%H:%M:%SZ")
        return ts.strftime('%Y-%m-%d %H:%M:%S.0')
    else:
        return r'2000-01-01 12:01:01.0'

def clean_id(_id):
    if type(_id) == int:
        return _id
    if type(_id) != int:
        parts = _id.split('-')
        for part in parts:
            try:
                v = int(part)
                return v
            except:
                pass
    if type(_id) != int:
        print('id', _id)
        return None

def write_user(user):
    res = []
    for col_name in user_col_names:
        if col_name == 'is_pro':
            is_pro = False
            for sub in user.get('creator_subscriptions', []):
                if sub.get('product', {}).get('id') == 'creator-pro':
                    is_pro = True
                    break
            res.append('true' if is_pro else 'false')
        elif col_name in ('last_modified', 'created_at'):
            res.append(translate_timestamp(user.get(col_name)))
        elif col_name == 'id' or '_id' in col_name:
            _id = clean_id(user['id'])
            if _id is None:
                return
            res.append(str(_id))
        else:
            v = user.get(col_name)
            if v is None:
                res.append(r'___')
            else:
                res.append(str(v))

    row_queue.put(('users', serialize_cols(res)))


def write_track(track):
    res = []
    for col_name in track_col_names:
        if col_name in track_timestamp_cols:
            res.append(translate_timestamp(track.get(col_name)))
        elif col_name in track_boolean_cols:
            v = track.get(col_name)
            res.append('true' if v is True else 'false' if v is False else '___')
        elif col_name == 'id' or '_id' in col_name:
            res.append(clean_id(track.get(col_name)))
        else:
            res.append(track.get(col_name))

    row_queue.put(('tracks', serialize_cols(res)))

def run_worker(proc_id, batch):
    unzip = sp.Popen(['xzcat'] + batch, stdout=sp.PIPE)
    for row in unzip.stdout:
        try:
            data = json.loads(row.strip())

            if 'links' in data:
                uid = data['user_id']
                for link in data['links']:
                    row_queue.put(('links', '%d\t%s\t%s\t%s\t%s' % (
                        uid, link.get('url', r'___'),
                        link.get('username', r'___'), link.get('network', r'___'),
                        link.get('title', r'___'))))
            elif 'likes' in data:
                uid = data['user_id']
                for like in data['likes']:
                    if 'playlist' in like:
                        continue #TODO: add playlists
                    if 'track' not in like:
                        print(like)
                    track = like['track']
                    track_user = track['user']
                    write_user(track_user)
                    write_track(track)
                    if type(track['id']) != int:
                            print(track)
                            continue
                    row_queue.put(('likes', '%r\t%r\t%s\t%s' % (uid, clean_id(track['id']), translate_timestamp(track['created_at']), track.get('kind', '___'))))
            elif 'followers' in data:
                from_id = data['user_id']
                for user in data['followers']:
                    write_user(user)
                    row_queue.put(('following', '%r\t%r' % (user['id'], from_id)))
            elif 'followings' in data:
                from_id = data['user_id']
                for user in data['followings']:
                    write_user(user)
                    row_queue.put(('following', '%r\t%r' % (from_id, user['id'])))
        except:
            traceback.print_exc()
            continue


for proc_id, batch in enumerate(fname_batches):
    if os.fork() == 0:
        try:
            run_worker(proc_id, batch)
        finally:
            row_queue.put('exit')
            sys.exit()

read_queue()
os.close(links_w)
os.close(users_w)
os.close(following_w)
