#!/usr/bin/env python3

#
# (c) Jared Weakly 2017
#
# This file will be a utility to help facilitate the comparison of performance
# metrics across arbitrary commits. The file will produce a table comparing
# metrics between measurements taken for given commits in the environment given
# by --test-env.

from __future__ import print_function

# TODO: Actually figure out what imports I need.
import argparse
import re
import os
import string
import subprocess

from testutil import parse_git_notes

parser = argparse.ArgumentParser()
parser.add_argument("--test-env",
                    help="The given test environment to be compared.")
parser.add_argument("--test-name",
                    help="Optional: If given, filters table to include only \
                    tests matching the given regular expression.")
# This is always going to be the last processing done on the metrics list because of how destructive it is.
parser.add_argument("--min-delta",
                    help="Optional: Display only tests where the relative \
                    spread is greater than the given value.")
parser.add_argument("--add-note", nargs=2,
                    help="Development only. Adds N fake metrics to the given commit")
parser.add_argument("commits", nargs=argparse.REMAINDER,
                    help="The rest of the arguments will be the commits that will be used.")

args = parser.parse_args()

# Defaults (can I avoid doing this?)
env = 'local'
name = re.compile('.*')
# metrics is a dictionary of the form
# {commit_1 : parse_git_notes, commit_2 : parse_git_notes, ... }
metrics = {}

if args.commits:
    metrics = dict(zip(args.commits, [parse_git_notes('perf',[c]) for c in args.commits]))

if args.test_env:
    temp = []
    for commit in metrics:
        temp.append([test for test in metrics.get(commit) if test['TEST_ENV'] == args.test_env])

    metrics = dict(zip(metrics.keys(),temp))

if args.test_name:
    name = re.compile(args.test_name)
    temp = []
    for commit in metrics:
        temp.append([test for test in metrics.get(commit) if name.search(test.get('TEST',''))])

    metrics = dict(zip(metrics.keys(),temp))

if args.min_delta:
    delta = 1.0 - float(args.min_delta)

    def cmp(v1, v2):
        return (v1/v2) > delta

    # I only want to compare the first commit with everything else.
    # So go through every item in the first commit and look up that test in
    # the other commits. Keep the falses.
    m = [] # tempy variable because I don't know how to do this better without internet.
    for tst in metrics.get(list(metrics.keys())[0]):
        for k in list(metrics.keys())[1:]: # Because I needed a separate iterator for this somehow
            for t in metrics.get(k):
                if (t['TEST'] == tst['TEST']):
                    m.append((tst,t)) # Pairing off matching test names across commits.

    deltas = []
    for k,v in m:
        # So... Much... Casting... oh my gawd.
        if (not cmp(float(k['VALUE']),float(v['VALUE']))):
            deltas.append(k)

    # metrics :: { commit : [ {tests} ] }
    metrics = { list(metrics.keys())[0] : deltas }

if args.add_note:
    def note_gen(n, commit):
        note = []
        # To generate good testing data, I need some similar test names, test metrics, environments in every commit.
        # I also need some same tests but different metric values, same test name, different test environment names, etc.
        # This will do for now, but it's not really sufficient.
        # There's a better test_metrics = {} dictionary I just stuck in a file for now.
        [note.append('\t'.join(['local', 'T'+ str(i*100), 'some_way', 'some_field', str(i*1000)])) for i in range(1,int(n)+1)]
        git_note = subprocess.check_output(["git","notes","--ref=perf","append",commit,"-m", "\n".join(note)])

    note_gen(args.add_note[0],args.add_note[1])


# At this point, since metrics is a { commit : [lst of tests] } variable,
# it should be pretty workable hopefully.
print(metrics)

# It'll be best to go through the newest commit (the one we care about)
# and find all matching TESTS and then make a table of comparisons.
# If there's a test in the newest commit that doesn't exist in older ones,
#  - should I just ignore it?
#  - should I just print it as a one line table?
#  -
# TEST: tst | METRIC: mtrk
# ----------+------------
#  commit1  | value
#  commit2  | value
#    ...    |  ...
#  commitN  | value

print("{:<12} {:<10} {:<10} {:<20} {:<15}".format('TEST_ENV','TEST','WAY','METRIC','VALUE'))
for key in metrics:
    print("{:<12} {:<10} {:<10} {:<20} {:<15}"
          .format(key['TEST_ENV'],key['TEST'],key['WAY'],key['METRIC'],key['VALUE']))
