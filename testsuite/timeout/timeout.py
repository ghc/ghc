#!/usr/bin/env python

try:

    import errno
    import os
    import signal
    import sys
    import time

    secs = int(sys.argv[1])
    cmd = sys.argv[2]

    def killProcess(pid):
        os.killpg(pid, signal.SIGKILL)
        for x in range(10):
            try:
                time.sleep(0.3)
                r = os.waitpid(pid, os.WNOHANG)
                if r == (0, 0):
                    os.killpg(pid, signal.SIGKILL)
                else:
                    return
            except OSError, e:
                if e.errno == errno.ECHILD:
                    return
                else:
                    raise e

    pid = os.fork()
    if pid == 0:
        # child
        os.setpgrp()
        os.execvp('/bin/sh', ['/bin/sh', '-c', cmd])
    else:
        # parent
        def handler(signum, frame):
            sys.stderr.write('Timeout happened...killing process...\n')
            killProcess(pid)
            sys.exit(99)
        old = signal.signal(signal.SIGALRM, handler)
        signal.alarm(secs)
        (pid2, res) = os.waitpid(pid, 0)
        if (os.WIFEXITED(res)):
            sys.exit(os.WEXITSTATUS(res))
        else:
            sys.exit(res)

except KeyboardInterrupt:
    sys.exit(98)
except:
    raise

