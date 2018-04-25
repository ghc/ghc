#!/usr/bin/env python

import utils, sys, codecs

def sort(filename):
    content = open(filename, encoding='utf-8').read()
    lines = content.splitlines()
    lines.sort()
    print('\n'.join(lines))

for f in sys.argv[1:]:
    t = utils.benchmark(lambda: sort(f))
    sys.stderr.write('{0}: {1}\n'.format(f, t))
