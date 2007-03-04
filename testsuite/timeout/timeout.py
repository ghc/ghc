#!/usr/bin/python

import os
import signal
import sys

secs = int(sys.argv[1])
cmd = sys.argv[2]

pid = os.fork()
# XXX error checking
if pid == 0:
    # child
    os.setpgrp()
    os.execvp('/bin/sh', ['/bin/sh', '-c', cmd])
else:
    # parent
    def handler(signum, frame):
        sys.stderr.write('Timeout happened...killing process...\n')
        os.killpg(pid, signal.SIGKILL) # XXX Kill better like .hs
        sys.exit(99)
    old = signal.signal(signal.SIGALRM, handler)
    signal.alarm(secs)
    (pid2, res) = os.waitpid(pid, 0)
    if (os.WIFEXITED(res)):
        sys.exit(os.WEXITSTATUS(res))
    else:
        sys.exit(res)

