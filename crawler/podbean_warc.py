import subprocess as sp
import sys, os

url_pattern = "https://www.podbean.com/site/userCenter/followMore/blog/%d"
def url(i):
    return url_pattern % i

max_concurrent = 20
step_size = 10000
procs = []
current_proc = None
i = 3172400

outf = os.path.abspath(sys.argv[1])

def make_proc(i):
    return sp.Popen(['wget', '-i', '-', '--delete-after', '--warc-file=%s.%d' % (outf, i), '--warc-cdx=on'], stdin=sp.PIPE, cwd='/tmp')

proc_size = 0
while i < 4760000:
    if current_proc is None:
        current_proc = make_proc(i)
        procs.append(current_proc)
    current_proc.stdin.write(url(i) + '\n')
    proc_size += 1

    if proc_size >= step_size:
        current_proc.stdin.close()
        while len(procs) >= max_concurrent:
            for idx, proc in enumerate(procs):
                try:
                    proc.wait()
                    procs.pop(idx)
                    break
                except sp.TimeoutException:
                    continue
        current_proc = make_proc(i)
        procs.append(current_proc)
        proc_size = 0

    i += 1
