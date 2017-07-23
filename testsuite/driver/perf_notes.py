#!/usr/bin/env python3

#
# (c) Jared Weakly 2017
#
# This file will be a utility to help facilitate the comparison of performance
# metrics across arbitrary commits. The file will produce a table comparing
# metrics between measurements taken for given commits in the environment given
# by --test-env.
<<<<<<< HEAD
=======

from __future__ import print_function
>>>>>>> Cleaning up my trash code for the perf_notes comparison tool

import argparse
import re
import subprocess

from testutil import parse_git_notes

parser = argparse.ArgumentParser()
parser.add_argument("--test-env",
                    help="The given test environment to be compared.")
parser.add_argument("--test-name",
                    help="Optional: If given, filters table to include only \
                    tests matching the given regular expression.")
parser.add_argument("--min-delta",type=float,
                    help="Optional: Display only tests where the relative \
                    spread is greater than the given value.")
parser.add_argument("--add-note", nargs=3,
                    help="Development only. Adds N fake metrics to the given commit. \
                    If the third argument is not a blank string, this will generate \
                    different looking fake metrics.")
parser.add_argument("commits", nargs=argparse.REMAINDER,
                    help="The rest of the arguments will be the commits that will be used.")
args = parser.parse_args()

#
# Defaults and utilities
#

env = 'local'
name = re.compile('.*')
# metrics is a dictionary of the form
# [ {'test_env': 'local', 'test': 'T100', 'way': 'some_way', 'metric': 'some_field', 'value': '1000', 'commit': 'HEAD'} ]
metrics = []

#
# Main logic of program
#

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

    def cmp(v1, v2):
        if v1 > v2:
            return (100 * (v1 - v2)/v2) > delta
        else:
            return (100 * (v2 - v1)/v1) > delta

    m = []
    for t in latest_commit:
        m += [(t,test) for test in metrics if (t['test'] == test['test']) and (t['commit'] != test['commit'])]

    deltas = []
    for fst,snd in m:
        if cmp(float(fst['value']),float(snd['value'])):
            deltas.append(fst)

    metrics = deltas

if args.add_note:
    def note_gen(n, commit, delta=''):
        note = []
        # Generates simple fake data. Likely not comprehensive enough to catch all edge cases.
        if not delta:
            [note.append('\t'.join(['local', 'T'+ str(i*100), 'some_way', 'some_field', str(i*1000)])) for i in range(1,int(int(n)/2)+1)]
            [note.append('\t'.join(['non-local', 'W'+ str(i*100), 'other_way', 'other_field', str(i*100)])) for i in range(int(int(n)/2)+1,int(n)+1)]
        if delta:
            [note.append('\t'.join(['local', 'T'+ str(i*100), 'some_way', 'some_field', str(i*10)])) for i in range(1,int(int(n)/2)+1)]
            [note.append('\t'.join(['non-local', 'W'+ str(i*100), 'other_way', 'other_field', str(i*1)])) for i in range(int(int(n)/2)+1,int(n)+1)]

        git_note = subprocess.check_output(["git","notes","--ref=perf","append",commit,"-m", "\n".join(note)])

    note_gen(args.add_note[0],args.add_note[1],args.add_note[2])

#
# String utilities for pretty-printing
#

string = ''
for i in args.commits:
    string+='{:18}'
commits = string.format(*[c[:10] for c in args.commits])
latest_commit = [test for test in metrics if test['commit'] == args.commits[0]]

def cmtline(insert):
    return string.format(*[insert for c in args.commits]).strip()

def header(unit):
    first_line = "{:27}{:30}".format('    ','      ') + cmtline(unit)
    second_line = ("{:27}{:30}".format('Test','Metric') + commits).strip()

    # Test   Metric   c1   c2   c3 ...
    print("-" * (len(second_line)+1))
    print(first_line)
    print(second_line)
    print("-" * (len(second_line)+1))

def commit_string(test, flag):
    def delta(v1, v2):
        return round((100 * (v1 - v2)/v2),2)

    i = 0
    string = []
    fmtstr = ""
    for commit in args.commits:
        fmtstr+="{:18}"
        string += [t['value'] for t in metrics if t['commit'] == args.commits[i] and t['test'] == test]
        i+=1
        string = string[:i]

    if flag == 'metrics':
        return fmtstr.format(*string).strip()
    if flag == 'percentages':
        s = [str(delta(float(string[0]),float(val))) + '%' for val in string]
        return fmtstr.format(*s).strip()

#
# The pretty-printed output
#

header('commit')
# Printing out metrics.
for test in latest_commit:
    print("{:27}{:30}".format(test['test'], test['metric']) + commit_string(test['test'],'metrics'))

header('percent')
# Printing out percentages.
for test in latest_commit:
    print("{:27}{:30}".format(test['test'], test['metric']) + commit_string(test['test'],'percentages'))
