#!/usr/bin/python3

try:
    import ujson as json
except ImportError:
    import json

def get_pairs(blob):
    if 'html_data' in blob:
        from_id = blob['podcast_id']
        to_ids = blob['html_data']['pageData']['podcastPageData']['listenersAlsoBought']
        return [(from_id, v) for v in to_ids]
    else:
        return []

if __name__ == '__main__':
    import sys
    for row in sys.stdin:
        blob = json.loads(row.strip())
        for k, v in get_pairs(blob):
            print('%s %s' % (k, v))

