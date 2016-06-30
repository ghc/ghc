#!/usr/bin/env python

from __future__ import print_function
import os.path
import sys
import re
import glob
import io
from collections import defaultdict

# keyed on unique type, values are lists of (unique, name) pairs
def find_uniques(source_files):
    uniques = defaultdict(lambda: defaultdict(lambda: set()))
    unique_re = re.compile(r"([\w\d]+)\s*=\s*mk([\w\d']+)Unique\s+(\d+)")
    for f in source_files:
        ms = unique_re.findall(io.open(f, encoding='utf8').read())
        for m in ms:
            name = m[0]
            _type = m[1]
            n = int(m[2])
            uniques[_type][n].add(name)

    return uniques

def print_all(uniques):
    for _type, uniqs in uniques.items():
        print('{_type} uniques'.format(**locals()))
        for n,names in uniqs.items():
            all_names = ', '.join(names)
            print('  {n} = {all_names}'.format(**locals()))

def find_conflicts(uniques):
    return [ (uniqueType, number, names)
             for uniqueType, uniqs in uniques.items()
             for number, names in uniqs.items()
             if len(names) > 1
           ]

top_dir = sys.argv[1]
uniques = find_uniques(glob.glob(os.path.join(top_dir, 'compiler', 'prelude', '*.hs')))
#print_all(uniques)
conflicts = find_conflicts(uniques)
if len(conflicts) > 0:
    print("Error: check-uniques: Found Unique conflict")
    print()
    for (ty, n, names) in conflicts:
        print('    %s unique %d conflict: %s' % (ty, n, ', '.join(names)))
    print()
    sys.exit(1)
