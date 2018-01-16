#!/usr/bin/env python

import utils, sys

def strip_tags(filename):
    string = open(filename, encoding='utf-8').read()

    d = 0
    out = []

    for c in string:
        if c == '<': d += 1

        if d > 0:
            out += ' '
        else:
            out += c

        if c == '>': d -= 1

    print(''.join(out))

for f in sys.argv[1:]:
    t = utils.benchmark(lambda: strip_tags(f))
    sys.stderr.write('{0}: {1}\n'.format(f, t))
