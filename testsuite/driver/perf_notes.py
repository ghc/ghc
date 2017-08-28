#!/usr/bin/env python3

#
# (c) Jared Weakly 2017
#
# This file will be a utility to help facilitate the comparison of performance
# metrics across arbitrary commits. The file will produce a table comparing
# metrics between measurements taken for given commits in the environment given
# by --test-env.
#

import argparse
import re
import subprocess

from testglobals import *
from math import ceil, trunc
from testutil import parse_git_notes

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--test-env",
                        help="The given test environment to be compared.")
    parser.add_argument("--test-name",
                        help="Optional: If given, filters table to include only \
                        tests matching the given regular expression.")
    parser.add_argument("--min-delta",type=float,
                        help="Optional: Display only tests where the relative \
                        spread is greater than the given value. \
                        This will not be run if you only pass in one commit.")
    parser.add_argument("--add-note", nargs=3,
                        help="Development only. Adds N fake metrics to the given commit. \
                        If the third argument is not a blank string, this will generate \
                        different looking fake metrics.")
    parser.add_argument("commits", nargs=argparse.REMAINDER,
                        help="The rest of the arguments will be the commits that will be used.")
    args = parser.parse_args()

    env = 'local'
    name = re.compile('.*')
    # metrics is a dictionary of the form
    # [ {'test_env': 'local', 'test': 'T100', 'way': 'some_way', 'metric': 'some_field', 'value': '1000', 'commit': 'HEAD'} ]
    metrics = []
    singleton_commit = len(args.commits) == 1

    #
    # Main logic of the program when called from the command-line.
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

        # Throw away the work if we only have one commit passed in.
        # Ugly way to do it but ¯\_(ツ)_/¯
        if not singleton_list:
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
# Comparison tools for the test driver to use on performance tests.
#

testing_metrics = ['bytes allocated', 'peak_megabytes_allocated', 'max_bytes_used']

def my_passed(reason=''):
    return {'passFail': 'pass', 'reason' : reason}

def my_failBecause(reason, tag=None):
    return {'passFail': 'fail', 'reason': reason, 'tag': tag}

# At some point this should be changed to handle tests like so:
# - Upon noticing a: 5% regression, leave a comment on phabricator
# -                 10% regression, flag commit for review on phabricator
# -                 20% regression, fail test.
def test_cmp(full_name, field, val, expected, dev=20):
    result = my_passed()
    lowerBound = trunc(           int(expected) * ((100 - float(dev))/100))
    upperBound = trunc(0.5 + ceil(int(expected) * ((100 + float(dev))/100)))
    deviation = round(((float(val) * 100)/ int(expected)) - 100, 1)

    if val < lowerBound:
        result = my_failBecause('value is too low:\n(If this is \
        because you have improved GHC, please\nupdate the test so that GHC \
        doesn\'t regress again)')
    if val > upperBound:
        result = my_failBecause('value is too high:\nstat is not good enough')

    if val < lowerBound or val > upperBound or config.verbose >= 4:
        length = max(len(str(x)) for x in [expected, lowerBound, upperBound, val])

        def display(descr, val, extra):
            print(descr, str(val).rjust(length), extra)

        display('    Expected    ' + full_name + ' ' + field + ':', expected, '+/-' + str(dev) + '%')
        display('    Lower bound ' + full_name + ' ' + field + ':', lowerBound, '')
        display('    Upper bound ' + full_name + ' ' + field + ':', upperBound, '')
        display('    Actual      ' + full_name + ' ' + field + ':', val, '')
        if val != expected:
            display('    Deviation   ' + full_name + ' ' + field + ':', deviation, '%')

    return result

# To be used instead of stats_num_field
# All defaults set for ease of use.
# Defaults to "test everything, only break on extreme cases, not a compiler test"
def comparison(metric='all', deviation=20, compiler=False):
    return lambda name, opts, m=metric, d=deviation, c=compiler: _comparison(name, opts, m, d, c)

def _comparison(name, opts, metric, deviation, is_compiler_test):
    tests = parse_git_notes('perf','HEAD^')

    # Might have multiple metrics being measured for a single test.
    test = [t for t in tests if t['test'] == name]

    if tests == [] or test == []:
        # There are no prior metrics for this test.
        if isinstance(metric, str):
            if metric == 'all':
                for field in testing_metrics:
                    opts.stats_range_fields[field] = (0,0)
            else:
                opts.stats_range_fields[metric] = (0,0)
        if isinstance(metric, list):
            for field in metric:
                opts.stats_range_fields[field] = (0,0)

        return

    if is_compiler_test:
        opts.is_compiler_test = True

    # Compiler performance numbers change when debugging is on, making the results
    # useless and confusing. Therefore, skip if debugging is on.
    if config.compiler_debugged and is_compiler_test:
        opts.skip = 1

    # 'all' is a shorthand to test for bytes allocated, peak megabytes allocated, and max bytes used.
    if isinstance(metric, str):
        if metric == 'all':
            for field in testing_metrics:
                opts.stats_range_fields[field] = ([t['value'] for t in test if t['metric'] == field][0], deviation)
                return
        else:
            opts.stats_range_fields[metric] = ([t['value'] for t in test if t['metric'] == metric][0], deviation)
            return

    if isinstance(metric, list):
        for field in metric:
            opts.stats_range_fields[field] = ([t['value'] for t in test if t['metric'] == field][0], deviation)

def evaluate_metric(opts, test, field, deviation, contents, way):
    full_name = test + ' (' + way + ' )'
    (expected,_) = opts.stats_range_fields[field]

    m = re.search('\("' + field + '", "([0-9]+)"\)', contents)
    if m == None:
        print('Failed to find field: ', field)
        return my_failBecause('no such stats field')

    val = int(m.group(1))

    # Add val into the git note if option is set.
    if config.use_git_notes:
        test_env = config.test_env
        config.accumulate_metrics.append('\t'.join([test_env, test, way, field, str(val)]))

    if expected == 0:
        return my_passed('no prior metrics for this test')

    return test_cmp(full_name, field, val, expected, deviation)

#
# String utilities for pretty-printing
#
def main_2():
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

def main_3():
    header('commit')
    # Printing out metrics.
    for test in latest_commit:
        print("{:27}{:30}".format(test['test'], test['metric']) + commit_string(test['test'],'metrics'))

    # Has no meaningful output if there is no commit to compare to.
    if not singleton_commit:
        header('percent')

        # Printing out percentages.
        for test in latest_commit:
            print("{:27}{:30}".format(test['test'], test['metric']) + commit_string(test['test'],'percentages'))

if __name__ == '__main__':
    main()
    main_2()
    main_3()
