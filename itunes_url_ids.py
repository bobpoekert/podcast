#!/usr/bin/python3
try:
    import ujson as json
except ImportError:
    import json
import re
def quote(v):
    return re.sub(r'\s', '', v.decode('utf-8'))

def get_urls_and_ids(blob):
    if 'html_data' in blob:
        _id = blob['podcast_id']
        url = blob['html_data']['pageData']['podcastPageData']['websiteUrl']
        if url is not None:
            return [(_id, quote(url.encode('utf-8'))), (_id, 'itunes:%s' % _id)]
        else:
            return [(_id, 'itunes:%s' % _id)]
    else:
        res = []
        for _id, body in blob['results'].items():
            if 'websiteUrl' in body:
                res.append((_id, quote(body['websiteUrl'].encode('utf-8'))))
            res.append((_id, 'itunes:%s' % _id))
        return res

if __name__ == '__main__':
    import sys, traceback
    for row in sys.stdin:
        try:
            blob = json.loads(row.strip())
            for _id, url in get_urls_and_ids(blob):
                print('%s %s' % (_id, url))
        except:
            traceback.print_exc()
