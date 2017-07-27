#!/usr/bin/env python3

#
# (c) Jared Weakly 2017
#
# This file will be a utility to help facilitate the comparison of performance
# metrics across arbitrary commits. The file will produce a table comparing
# metrics between measurements taken for given commits in the environment given
# by --test-env.

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
parser.add_argument("--min-delta",type=float,
                    help="Optional: Display only tests where the relative \
                    spread is greater than the given value.")
parser.add_argument("--add-note", nargs=3,
                    help="Development only. Adds N fake metrics to the given commit. \
                    The third argument is a useless flag to add some functionality.")
parser.add_argument("commits", nargs=argparse.REMAINDER,
                    help="The rest of the arguments will be the commits that will be used.")

args = parser.parse_args()

# Defaults (can I avoid doing this?)
env = 'local'
name = re.compile('.*')
# metrics is a dictionary of the form
# [ {'test_env': 'local', 'test': 'T100', 'way': 'some_way', 'metric': 'some_field', 'value': '1000', 'commit': 'HEAD'} ]
metrics = []

if args.commits:
    for c in args.commits:
        metrics += parse_git_notes('perf',c)

if args.test_env:
    metrics = [test for test in metrics if test['test_env'] == args.test_env]

if args.test_name:
    name = re.compile(args.test_name)
    metrics = [test for test in metrics if name.search(test.get('test',''))]

if args.min_delta:
    delta = args.min_delta
    print(delta)

    # Took me way too long to realize I had the math utterly borked for the comparison
    def cmp(v1, v2):
        if v1 > v2:
            return (100 * (v1 - v2)/v2) > delta
        else:
            return (100 * (v2 - v1)/v1) > delta

    # I only want to compare the first commit with everything else.
    # So go through every item in the first commit and look up that test in
    # the other commits. Keep the falses.
    latest_commit = [t for t in metrics if t['commit'] == args.commits[0]]

    m = [] # tempy variable
    for t in latest_commit:
        m += [(t,test) for test in metrics if (t['test'] == test['test']) and (t['commit'] != test['commit'])]

    deltas = []
    for fst,snd in m:
        # So... Much... Casting... oh my gawd.
        if cmp(float(fst['value']),float(snd['value'])):
            deltas.append(fst)

    metrics = deltas

if args.add_note:
    def note_gen(n, commit, delta=''):
        note = []
        # To generate good testing data, I need some similar test names, test metrics, environments in every commit.
        # I also need some same tests but different metric values, same test name, different test environment names, etc.
        # This will do for now, but it's not really sufficient.
        # There's a better test_metrics = {} dictionary I just stuck in a file for now.
        if not delta:
            [note.append('\t'.join(['local', 'T'+ str(i*100), 'some_way', 'some_field', str(i*1000)])) for i in range(1,int(int(n)/2)+1)]
            [note.append('\t'.join(['non-local', 'W'+ str(i*100), 'other_way', 'other_field', str(i*100)])) for i in range(int(int(n)/2)+1,int(n)+1)]
        if delta:
            [note.append('\t'.join(['local', 'T'+ str(i*100), 'some_way', 'some_field', str(i*10)])) for i in range(1,int(int(n)/2)+1)]
            [note.append('\t'.join(['non-local', 'W'+ str(i*100), 'other_way', 'other_field', str(i*1)])) for i in range(int(int(n)/2)+1,int(n)+1)]

        git_note = subprocess.check_output(["git","notes","--ref=perf","append",commit,"-m", "\n".join(note)])

    note_gen(args.add_note[0],args.add_note[1],args.add_note[2])

latest_commit = [t for t in metrics if t['commit'] == args.commits[0]]
rest = [t for t in metrics if t['commit'] != args.commits[0]]

for test in latest_commit:
    print("{:<13} {:5} {:<13}".format('TEST: ' + test['test'], ' | ', 'METRIC: ' + test['metric']))
    print("-------------------------------")
    print("{:<13} {:5} {:<13}".format('commit:' + test['commit'], ' | ', test['value']))
    for t in rest:
        if t['test'] == test['test']:
            print("{:<13} {:5} {:<13}".format('commit:' + t['commit'], ' | ', t['value']))
    print('\n')
