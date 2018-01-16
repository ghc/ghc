#!/usr/bin/env python

import utils, sys, codecs

def cut(filename, l, r):
    content = open(filename, encoding='utf-8')
    for line in content:
        print(line[l:r])

for f in sys.argv[1:]:
    t = utils.benchmark(lambda: cut(f, 20, 40))
    sys.stderr.write('{0}: {1}\n'.format(f, t))
