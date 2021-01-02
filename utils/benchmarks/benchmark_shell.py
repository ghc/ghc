#!/usr/bin/python
"""
This program is intended to be run via sudo, and does the following
things:

1. Sets up a high-performance networking environment by tweaking
   various sysctl settings.
2. Runs either an interactive shell or a command line program.
3. Resets the environment back to what it was.
"""

import os
import sys
import tempfile

SYSCTLS = dict(
    Darwin={
        'kern.ipc.somaxconn': 1024,
        'kern.maxfiles': 22528,
        'kern.maxfilesperproc': 20480,
        'net.inet.ip.portrange.first': 1024,
        'net.inet.ip.portrange.hifirst': 1024,
        },
    Linux={
        'net.core.somaxconn': 1024,
        'net.core.rmem_max': 16777216,
        'net.core.wmem_max': 16777216,
        'net.ipv4.ip_local_port_range': '1024 65535',
        'net.ipv4.tcp_fin_timeout': 15,
        'net.ipv4.tcp_max_syn_backlog': 16384,
        'net.ipv4.tcp_rmem': '4096 87380 16777216',
        'net.ipv4.tcp_tw_recycle': 1,
        'net.ipv4.tcp_tw_reuse': 1,
        'net.ipv4.tcp_wmem': '4096 65536 16777216',
        },
    )

ULIMITS = dict(
    Darwin={
        '-n': 20480,
        },
    Linux={
        '-n': 131072,
        },
    )

if os.access('/sbin/sysctl', os.X_OK):
    SYSCTL = '/sbin/sysctl'
elif os.access('/usr/sbin/sysctl', os.X_OK):
    SYSCTL = '/usr/sbin/sysctl'
else:
    print >> sys.stderr, 'where is sysctl!?'
    sys.exit(1)

CHANGED_SYSCTLS = {}

def change_sysctl(name, newval):
    """ Set kernel parameters using sysctl for the appropriate platform """
    oldval = os.popen('%s -n %s' % (SYSCTL, name), 'r').read().strip().replace('\t', ' ')
    if not oldval:
        print('could not get value of ' + name, file=sys.stderr)
        return
    if oldval == str(newval):
        return
    ret = os.system('%s -w %s=%r 2>/dev/null' % (SYSCTL, name, newval))
    if ret != 0:
        print('could not change %s from %s to %r' % (name, oldval, newval),
                file=sys.stderr)
        return
    CHANGED_SYSCTLS[name] = oldval


PLATFORM = os.uname()[0]

for (n, v) in SYSCTLS[PLATFORM].items():
    change_sysctl(n, v)

FD, PATH = tempfile.mkstemp('.sh')
FP = os.fdopen(FD, 'w')

for (n, v) in ULIMITS[PLATFORM].items():
    print('ulimit %s %s' % (n, v), file=FP)
    if len(sys.argv) > 1:
        print('exec ' + ' '.join(sys.argv[1:]), file=FP)
    else:
        print('exec %s -l' % os.environ.get('SHELL', '/bin/bash'), file=FP)

FP.close()
os.system('exec /bin/sh ' + PATH)
os.unlink(PATH)

for (n, v) in CHANGED_SYSCTLS.items():
    change_sysctl(n, v)
