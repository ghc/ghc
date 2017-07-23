#1/usr/bin/env python3

#
# (c) Jared Weakly 2017
#
# This file will be a utility to help facilitate the comparison of performance
# metrics across arbitrary commits. The file will produce a table comparing
# metrics between measurements taken for given commits in the environment given
# by --test-env.
#
# The file will also (for now) exist as a library to import git-note
# functionality for the project into other files so everywhere has access to
# functions such as parse_git_notes.
#
# It will take a few arguments:
#    --test-env=
#    --test-name=  (optional: If given, filters table to include only tests matching the given regular expression.)
#    --min-delta=  (optional: Display only tests where the relative spread is greater than the given value.)
#    All following arguments will be the commits to compare.

from __future__ import print_function

# TODO: Actually figure out what imports I need.
import argparse
import re
import os
import string
import subprocess

from testutil import parse_git_notes

# --------- Comparison Utilities -------- #
parser = argparse.ArgumentParser()
parser.add_argument("--test-env",
                    help="The given test environment to be compared.") #,
                    # required=True) # Should I make this required?
parser.add_argument("--test-name",
                    help="Optional: If given, filters table to include only \
                    tests matching the given regular expression.")
# This is always going to be the last processing done on the metrics list because of how destructive it is.
parser.add_argument("--min-delta",
                    help="Optional: Display only tests where the relative \
                    spread is greater than the given value.")
parser.add_argument("--add-note", nargs=2,
                    help="Development only. Adds N fake metrics to the given commit")
parser.add_argument("commits", nargs=argparse.REMAINDER)

args = parser.parse_args()

# Defaults
env = 'local'
name = re.compile('.*')
metrics = {} # metrics is actually best thought of as a dictionary
             # {commit_1 : parse_git_notes, commit_2 : parse_git_notes, ... }
             # Used to be just list, not dictionary. metrics.values() returns a list of the parse_git_notes lists.

# I should figure out a nice way to mark data with the commit it comes from
# so that I can display test performance numbers in order from oldest to newest commit.
if args.commits:
    # print(args.commits)
    metrics = dict(zip(args.commits, [parse_git_notes('perf',[c]) for c in args.commits]))

if args.test_env:
    env = args.test_env
    temp = []
    for commit in metrics:
        temp.append([test for test in metrics.get(commit) if test['TEST_ENV'] == env])

    metrics = dict(zip(metrics.keys(),temp))

if args.test_name:
    name = re.compile(args.test_name)
    temp = []
    temp.sort
    dic = {'a': 1}
    dic.update
    for commit in metrics:
        temp.append([test for test in metrics.get(commit) if name.search(test.get('TEST',''))])
    # metrics = [test for test in metrics if name.search(test.get('TEST',''))]
    metrics = dict(zip(metrics.keys(),temp))

if args.min_delta:
    delta = 1.0 - float(args.min_delta)

    def cmp(v1, v2):
        return (v1/v2) > delta

    # I'm an idiot, this is way easier than I was making it out to be.
    # I only want to compare the first commit with everything else.
    # So go through every item in the first commit and look up that test in
    # the other commits. Keep the falses.
    metrikz = []
    for tst in metrics.get(list(metrics.keys())[0]):
        for k in list(metrics.keys())[1:]:
            for cmt in metrics.get(k):
                if (cmt['TEST'] == tst['TEST']):
                    metrikz.append((tst,cmt))

    deltas = []
    for k,v in metrikz:
        if (not cmp(float(k['VALUE']),float(v['VALUE']))):
            deltas.append(k)

    metrics = { list(metrics.keys())[0] : deltas }
    # # Go through the nested dictionary.
    # # Pair off all same named tests per commit.
    # # [ [{c1 : {t1}}, {c2 : {t1}}, {c3 : {t1}}, ...],
    # #   [{c1 : {t2}}, {c2 : {t2}}, {c3 : {t2}}, ...], ... ]
    # # Assume commits are in order so that c1 is newest, cN is oldest.
    # # drill into structure. Compare. Keep all falses.
    # # Then put this back into the format needed.
    # mtks = []
    # for cmt in metrics.keys():
    #     for t in metrics.get(cmt):
    #         mtks.append(dict(zip(list(t) + ['COMMIT'], list(t.values()) + [cmt])))

    # groupedTests = []
    # for t1 in mkts:
    #     for t2 in mkts:
    #         if ((t1['TEST'] == t2['TEST']) and (t1['COMMIT'] != t2['COMMIT'])):
    #             groupedTests.append((t1,t2))


    # temp = []
    # # for commit in metrics:
    #     # temp.append([test for test in metrics.get(commit) if ])

if args.add_note:
    def note_gen(n, commit):
        note = []
        # To generate good testing data, I need some similar test names, test metrics, environments in every commit.
        # I also need some same tests but different metric values, same test name, different test environment names, etc.
        # This will do for now, but it's not really sufficient.
        [note.append('\t'.join(['local', 'T'+ str(i*100), 'some_way', 'some_field', str(i*1000)])) for i in range(1,int(n)+1)]
        git_note = subprocess.check_output(["git","notes","--ref=perf","append",commit,"-m", "\n".join(note)])

    note_gen(args.add_note[0],args.add_note[1])


# Logic should probably go here to sort, group, and otherwise prepare the list
# of dicts for being pretty printed.
print(metrics)

# I'll redo this table almost entirely, it's just a proof of concept for now.
# Ideally the list of metrics should be grouped by same test and organized from oldest to newest commits
# and each test will have its own small paragraph. I'm envisioning something like:
# --------------------------------
# Test Foo: test_env, test_way, metric
# ---------------------------------
# commit1 commit2 commit3 ...
# number1 number2 number3 ...
#
# Gosh, I want to just print a list of dictionaries pretty like but don't want to just add some random dependency...
# Table is hardcoded and pretty ugly, but... it works.
# For now, this table just pretty prints the list of dictionaries.
print("{:<12} {:<10} {:<10} {:<20} {:<15}".format('TEST_ENV','TEST','WAY','METRIC','VALUE'))
for key in metrics:
    print("{:<12} {:<10} {:<10} {:<20} {:<15}"
          .format(key['TEST_ENV'],key['TEST'],key['WAY'],key['METRIC'],key['VALUE']))
