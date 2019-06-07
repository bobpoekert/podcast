#!/usr/bin/env python3
import subprocess
import sys, os

with open('podbean_cookie.txt', 'r') as inf:
    cookie_header = 'Cookie: %s' % inf.read().strip()

n_workers = 20
out_path = '/mnt/lappy/podbean'

def proc(idx):
    prefix = '%s/podbean_channel' % out_path
    cdx_fname = '%s.%s.cdx' % (prefix, idx)
    if os.path.exists(cdx_fname):
        dedup = ['--warc-dedup=%s.%s.cdx' % (prefix, idx)]
    else:
        dedup = []
    return subprocess.Popen(['wget', '-t', '1', '-i', '-', '--delete-after', '--warc-file=%s.%d' % (prefix, idx), '--warc-cdx=on', '-T', '2', '--header', cookie_header] + dedup,
            stdin=subprocess.PIPE, cwd='/tmp')


urls = []
with open('podbean_page_urls.txt', 'r') as inf:
    for url in inf:
        urls.append('https' + url.strip())

print(len(urls))

partitions = []
partition_size = int(len(urls) / n_workers)
for off in range(0, len(urls), partition_size):
    partitions.append(urls[off:(off + partition_size)])

procs = []
for idx in range(n_workers):
    procs.append(proc(idx))

for proc, batch in zip(procs, partitions):
    for line in batch:
        proc.stdin.write((line + '\n').encode('utf-8'))
    proc.stdin.close()

for proc in procs:
    proc.wait()
