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

    id integer, customer_reviews_url text, n_customer_reviews integer, kind_ext_id text, kind_id integer,
    website_url text, kind_name text, feed_url text, artist_url text, itunes_notes text, description text,
    genres text, content_rating text, rating_counts text, name_sort_value text, created_on timestamp,
    name text, name_raw text, language_name text, artist_name text, is_news boolean

'''

user_cols = [v.strip().split() for v in user_cols_string.split(',')]
user_col_names = [v[0] for v in user_cols]

print(user_col_names)

track_cols_string = '''

    id bigint, podacst_id integer, feed_url text, release_date timestamp, description text, genres text,
    collection_name text, episode_guid text, content_rating text, short_url text, track_number integer, 
    episode_type text, itunes_title text, name text, name_raw text, itunes_url text, artist_name text,
    episote_website_url text

'''

track_cols = [v.strip().split() for v in track_cols_string.split(',')]
track_col_names = [v[0] for v in track_cols]

print(track_col_names)

review_cols_string = '''
    id bigint, podcast_id integer, body text, rating integer, name text, title text, vote_sum integer, vote_count integer,
    is_edited boolean, customer_type text, date timestamp
    '''

review_cols = [v.strip().split() for v in review_cols_string.split(',')]
review_col_names = [v[0] for v in review_cols]

cur = db.cursor()
cur.execute('create table if not exists itunes_podcasts (%s)' % user_cols_string)
cur.execute('create table if not exists itunes_podcast_episodes (%s)' % track_cols_string)
cur.execute('create table if not exists itunes_podcast_reviews (%s)' % review_cols_string)
db.commit()

def load_table(inp_pipe, table_name):
    uniq = sp.Popen(['hashuniq'], stdin=inp_pipe, stdout=sp.PIPE)
    copy = sp.Popen(['psql', '-c', 'copy %s from stdin delimiter \'\t\' null \'___\' encoding \'utf-8\'' % table_name], stdin=uniq.stdout)
    return copy


podcasts_r, podcasts_w = os.pipe()
episodes_r, episodes_w = os.pipe()
reviews_r, reviews_w = os.pipe()

tables_w = {
        'review':reviews_w,
        'podcast':podcasts_w,
        'user':podcasts_w,
        'track':episodes_w,
        'episode':episodes_w
        }

load_table(podcasts_r, 'itunes_podcasts')
load_table(episodes_r, 'itunes_podcast_episodes')
load_table(reviews_r, 'itunes_podcast_reviews')

row_queue = Queue(100)

def get_keys(m, k):
    for kk in k:
        try:
            v = m[kk]
        except KeyError:
            return None
        if type(v) is not dict:
            return v
        m = v
    return m

def read_queue():
    worker_count = n_workers
    ctr = 0
    while worker_count > 0:
        row = row_queue.get()
        ctr += 1
        if ctr % 10000 == 0:
            print(ctr, row)
        if row == 'exit':
            worker_count -= 1
            continue
        table_name, vals = row
        try:
            pipe = tables_w[table_name]
        except KeyError:
            print(table_name)
            continue
        os.write(pipe, ('%s\n' % vals).encode('utf-8'))

def expand_unicode(s):
    res = []
    for c in s:
        if c in 'qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890_-:. ':
            res.append(c)
        else:
            res.append(r'\\u%04x' % ord(c))
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

def process_tracks(data):
    if 'results' in data:
        data = data['results']
    for track_id, track_data in data.items():
        track = [
                track_id,
                track_data.get('collectionId'),
                track_data['feedUrl'],
                translate_timestamp(track_data['releaseDateTime']),
                get_keys(track_data, ('description', 'standard')),
                ','.join(v['name'] for v in track_data['genres']),
                track_data.get('collectionName'),
                track_data.get('podcastEpisodeGuid'),
                get_keys(track_data, ('contentRating', 'name')),
                track_data['shortUrl'],
                track_data.get('trackNumber'),
                track_data.get('podcastEpisodeType', None),
                track_data['name'],
                track_data['nameRaw'],
                track_data['url'],
                track_data['artistName'],
                track_data.get('podcastEpisodeWebsiteUrl', None),
                track_data.get('podcastEpisodeSeason', None)]
        row_queue.put(('track', serialize_cols(track)))


def run_worker(proc_id, batch):
    unzip = sp.Popen(['xzcat'] + batch, stdout=sp.PIPE)
    for row in unzip.stdout:
        try:
            data = json.loads(row.strip())
            if 'html_data' in data:
                podcast_id = data['podcast_id']
                page_data = data['html_data']['pageData']
                podcast_data = page_data['podcastPageData']
                platform_data = data['html_data']['storePlatformData']['product-dv-product']
                platform_item = platform_data['results'][str(podcast_id)]
                user = [
                        podcast_id,
                        podcast_data['customerReviewsUrl'],
                        podcast_data['totalNumberOfReviews'],
                        podcast_data['kindExtId'],
                        podcast_data['kindId'],
                        podcast_data['websiteUrl'],
                        podcast_data['kindName'],
                        platform_item['feedUrl'],
                        platform_item['artistUrl'],
                        platform_item['itunesNotes'],
                        get_keys(platform_item, ('description', 'standard')),
                        ','.join(v['name'] for v in platform_item['genres']),
                        get_keys(platform_item, ('contentRating', 'name')),
                        ','.join(str(v) for v in platform_item['userRating']['ratingCountList']),
                        platform_item['nameSortValue'],
                        translate_timestamp(platform_item['releaseDateTime']),
                        platform_item['name'],
                        platform_item['nameRaw'],
                        platform_item['podcastLanguageName'],
                        platform_item['artistName'],
                        platform_item['isNews']]
                
                row_queue.put(('user', serialize_cols(user)))

                if 'children' in platform_item:
                    process_tracks(platform_item['children'])

                for comment_data in podcast_data['userReviewList']:
                    comment = [
                            comment_data['userReviewId'],
                            podcast_id,
                            comment_data['body'],
                            comment_data['rating'],
                            comment_data['name'],
                            comment_data['title'],
                            comment_data['voteSum'],
                            comment_data['voteCount'],
                            comment_data['isEdited'],
                            comment_data['customerType'],
                            translate_timestamp(comment_data['date'])]
                    row_queue.put(('review', serialize_cols(comment)))

            else:
                process_tracks(data)

        except KeyboardInterrupt:
            return
        except:
            traceback.print_exc()
            return

for proc_id, batch in enumerate(fname_batches):
    if os.fork() == 0:
        try:
            run_worker(proc_id, batch)
        finally:
            row_queue.put('exit')
            sys.exit()

read_queue()
for v in tables_w.values():
    os.close(v)
