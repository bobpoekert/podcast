import subprocess as sp
import sys

url_pattern = "https://www.podbean.com/site/userCenter/followMore/blog/%d"
def url(i):
    return url_pattern % i

max_concurrent = 50
step_size = 10000
procs = []
for i in xrange(0, 4760000, step_size):
    idx = len(procs)
    getter = sp.Popen(['wget', '-i', '-', '--warc-file=%s.%d' % (sys.argv[1], idx), '--warc-cdx=on'], stdin=sp.PIPE)
    procs.append(getter)
    with getter.stdin:
        for j in range(i, i+step_size):
            getter.stdin.write(url(j) + '\n')

    if len(procs) >= max_concurrent:
        for proc in procs:
            proc.join()
        procs = []
