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
parser.add_argument("--min-delta",
                    help="Optional: Display only tests where the relative \
                    spread is greater than the given value.")
parser.add_argument("commits", nargs=argparse.REMAINDER)

args = parser.parse_args()

# Defaults
env = 'local'
name = re.compile('.*')
metrics = []

# I should figure out a nice way to mark data with the commit it comes from
# so that I can display test performance numbers in order from oldest to newest commit.
if args.commits:
    print(args.commits)
    metrics = parse_git_notes('perf',args.commits)

if args.test_env:
    env = args.test_env
    metrics = [test for test in metrics if test['TEST_ENV'] == env]

if args.test_name:
    name = re.compile(args.test_name)
    metrics = [test for test in metrics if name.search(test.get('TEST',''))]

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
