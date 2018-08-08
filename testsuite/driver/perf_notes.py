#!/usr/bin/env python3

#
# (c) Jared Weakly 2017
#
# This file will be a utility to help facilitate the comparison of performance
# metrics across arbitrary commits. The file will produce a table comparing
# metrics between measurements taken for given commits in the environment
# (which defaults to 'local' if not given by --test-env).
#

import argparse
import re
import subprocess
import time

from math import ceil, trunc
from testutil import passed, failBecause


#
# Some data access functions. A the moment this uses git notes.
#

# The metrics (a.k.a stats) are dictionaries in this form:
#
# { 'test_env' : 'val',      # Test environment.
#   'test'     : 'val',      # Name of the test 
#   'way'      : 'val',
#   'metric'   : 'val',      # Metric being recorded
#   'value'    : 'val',      # The statistic result e.g. runtime
#
#   # Optionally:
#   'commit'   : 'val',      # The ghc git commit
# }

# All the fields of a metric (no excluding commit field).
def perf_stat_fields():
    return ['test_env','test','way','metric','value']

# Crete a metric dict from a list of values matching the keys in perf_stat_fields().
def perf_stat_from_field_vals(field_vals):
    return dict(zip(perf_stat_fields(), field_vals))

def parse_perf_stat(stat_str, commit=None):
    field_vals = stat_str.strip('\t').split('\t')
    stat = perf_stat_from_field_vals(field_vals)
    if commit:
        stat['commit'] = commit

    return stat

# Get all recorded (in a git note) metrics for a given commit.
# Returns an empty array if the note is not found.
def get_perf_stats(commit='HEAD', namespace='perf'):
    try:
        log = subprocess.check_output(['git', 'notes', '--ref=' + namespace, 'show', commit], stderr=subprocess.STDOUT).decode('utf-8')
    except subprocess.CalledProcessError:
        return []

    log = log.strip('\n').split('\n')
    log = list(filter(None, log))
    log = [parse_perf_stat(stat_str, commit) for stat_str in log]
    return log


# Get allowed changes to performance. This is extracted from the commit message of
# the given commit in this form:
#     Metric  (Increase | Decrease)  ['metric' | \['metrics',..\]]  [\((test_env|way)='abc',...\)]: TestName01, TestName02, ...
# Returns a *dictionary* from test name to a *list* of items of the form:
#   {
#           'direction': either 'Increase' or 'Decrease,
#           'metrics': ['metricA', 'metricB', ...],
#           'opts': {
#                   'optionA': 'string value',
#                   'optionB': 'string value',
#                   ...
#               }
#   }
def get_allowed_perf_changes(commit='HEAD'):
    commitByteStr = subprocess.check_output(['git', '--no-pager', 'log', '-n1', '--format=%B', commit])
    return parse_allowed_perf_changes(commitByteStr.decode())

def parse_allowed_perf_changes(commitMsg):
    # Helper regex. Non-capturing unless postfixed with Cap.
    s = r"(?:\s*\n?\s+)"                                    # Space, possible new line with an indent.
    qstr = r"(?:'(?:[^'\\]|\\.)*')"                         # Quoted string.
    qstrCap = r"(?:'((?:[^'\\]|\\.)*)')"                    # Quoted string. Captures the string without the quotes.
    innerQstrList = r"(?:"+qstr+r"(?:"+s+r"?,"+s+r"?"+qstr+r")*)?"     # Inside of a list of strings.gs.s..
    qstrList = r"(?:\["+s+r"?"+innerQstrList+s+r"?\])"      # A list of strings (using box brackets)..

    exp = (r"^Metric"
        +s+r"(Increase|Decrease)"
        +s+r"?("+qstr+r"|"+qstrList+r")?"                   # Metric or list of metrics.s..
        +s+r"?(\(" + r"(?:[^')]|"+qstr+r")*" + r"\))?"      # Options surounded in parenthesis. (allow parenthases in quoted strings))
        +s+r"?:?"                                           # Optional ":"
        +s+r"?((?:(?!\n\n)(?!\n[^\s])(?:.|\n))*)"           # Test names. Stop parsing on empty or non-indented new line.
        )

    matches = re.findall(exp, commitMsg, re.M)
    changes = {}
    for (direction, metricsStr, optsStr, testsStr) in matches:
        tests = re.findall(r"(\w+)", testsStr)
        for test in tests:
            changes.setdefault(test, []).append({
                'direction': direction,
                'metrics': re.findall(qstrCap, metricsStr),
                'opts': dict(re.findall(r"(\w+)"+s+r"?="+s+r"?"+qstrCap, optsStr))
            })

    return changes

# Appends a list of metrics to the git note of the given commit.
# Tries up to max_tries times to write to git notes should it fail for some reason.
# Each retry will wait 1 second.
# Returns True if the note was successfully appended.
def append_perf_stat(stats, commit='HEAD', namespace='perf', max_tries=5):
    # If a single stat, convert to a singleton list.
    if not isinstance(stats, list):
        stats = [stats]

    # Append to git note
    print('Appending ' + str(len(stats)) + ' stats to git notes.')
    metrics = perf_stat_fields()
    stats_str = "\n".join(["\t".join([str(stat[metric]) for metric in metrics]) for stat in stats])
    def try_append():
            try:
                return subprocess.check_output(['git', 'notes', '--ref=perf', 'append', commit, '-m', stats_str])
            except subprocess.CalledProcessError:
                return b'Git - fatal'

    tries = 0
    while tries < max_tries:
        if not b'Git - fatal' in try_append():
            return True
        tries += 1
        time.sleep(1)

    print("\nAn error occured while writing the performance metrics to git notes.\n \
	​            This is usually due to a lock-file existing somewhere in the git repo.")

    return False

# Check test stats. This prints the results for the user.
# test: name of the test.
# metric: the metric to check.
# test_env: the test environment string.
# way
# expected_val: the expected value (this should generally be derived from get_perf_stats()).
# actual_val: the value observed in the current test run.
# tolerance_dev: allowed deviation of the actual value from the expected value.
# allowed_perf_changes: allowed changes in stats. This is a dictionary as returned by get_allowed_perf_changes().
# force_print: Print stats even if the test stat was in the tolerance range.
# Returns a pass/fail object. Passes if the stats are withing the expected value ranges.
def check_stats(test, metric, test_env, way, expected_val, actual_val, tolerance_dev, allowed_perf_changes = {}, force_print = False):

    full_name = test + ' (' + way + ')'

    lowerBound = trunc(           int(expected_val) * ((100 - float(tolerance_dev))/100))
    upperBound = trunc(0.5 + ceil(int(expected_val) * ((100 + float(tolerance_dev))/100)))

    actual_dev = round(((float(actual_val) * 100)/ int(expected_val)) - 100, 1)

    allowed_change_directions =  [ change['direction']
            for change in allowed_perf_changes.get(test, [])
            if metric in change['metrics']
            if ((not 'way'      in change['opts'].keys()) or way      == change['opts']['way'])
            if ((not 'test_env' in change['opts'].keys()) or test_env == change['opts']['test_env'])
        ]

    def print_help(direction):
        print()
        print('If this is an expected change, please add this in the git commit message:')
        print("    Metric " + direction + " '" + metric + "' (test_env='" + test_env + "', way='" + way + "'): " + test)
        print()

    print_help_dir = False
    result = passed()
    too_low = actual_val < lowerBound and not 'Decrease' in allowed_change_directions
    if too_low:
        print(metric, 'value is too low:')
        print_help_dir = 'Decrease'
        result = failBecause('stat too good', tag='stat')
    too_high = actual_val > upperBound and not 'Increase' in allowed_change_directions
    if too_high:
        print(metric, 'value is too high:')
        print_help_dir = 'Increase'
        result = failBecause('stat not good enough', tag='stat')

    if too_high or too_low or force_print:
        length = max(len(str(x)) for x in [expected_val, lowerBound, upperBound, actual_val])

        def display(descr, val, extra):
            print(descr, str(val).rjust(length), extra)

        display('    Expected    ' + full_name + ' ' + metric + ':', expected_val, '+/-' + str(tolerance_dev) + '%')
        display('    Lower bound ' + full_name + ' ' + metric + ':', lowerBound, '')
        display('    Upper bound ' + full_name + ' ' + metric + ':', upperBound, '')
        display('    Actual      ' + full_name + ' ' + metric + ':', actual_val, '')
        if actual_val != expected_val:
            display('    Deviation   ' + full_name + ' ' + metric + ':', actual_dev, '%')

    if print_help_dir:
        print_help(print_help_dir)

    return result

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("--test-env",
                        help="The given test environment to be compared.")
    parser.add_argument("--test-name",
                        help="If given, filters table to include only \
                        tests matching the given regular expression.")
    parser.add_argument("--add-note", nargs=3,
                        help="Development only. --add-note N commit seed \
                        Adds N fake metrics to the given commit using the random seed.")
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
            metrics += get_perf_stats(c)

    if args.test_env:
        metrics = [test for test in metrics if test['test_env'] == args.test_env]

    if args.test_name:
        nameRe = re.compile(args.test_name)
        metrics = [test for test in metrics if nameRe.search(test.get('test',''))]

    if args.add_note:
        def note_gen(n, commit, delta=''):
            note = []
            # Generates simple fake data. Likely not comprehensive enough to catch all edge cases.
            if not delta:
                note.extend([perf_stat_from_field_vals(['local', 'T'+ str(i*100), 'some_way', 'some_field', str(i*1000)]) for i in range(1,int(int(n)/2)+1)])
                note.extend([perf_stat_from_field_vals(['non-local', 'W'+ str(i*100), 'other_way', 'other_field', str(i*100)]) for i in range(int(int(n)/2)+1,int(n)+1)])
            if delta:
                hu = abs(hash(delta))
                hv = abs(hash(hu))
                u = int(hu % 100)
                v = int(hv % 10)
                note.extend([perf_stat_from_field_vals(['local', 'T'+ str(i*100), 'some_way', 'some_field', str(i*u)]) for i in range(1,int(int(n)/2)+1)])
                note.extend([perf_stat_from_field_vals(['non-local', 'W'+ str(i*100), 'other_way', 'other_field', str(i*v)]) for i in range(int(int(n)/2)+1,int(n)+1)])

            append_perf_stat(note, commit)

        note_gen(args.add_note[0],args.add_note[1],args.add_note[2])

    #
    # String utilities for pretty-printing
    #

    row_fmt = '{:18}' * len(args.commits)
    commits = row_fmt.format(*[c[:10] for c in args.commits])

    def cmtline(insert):
        return row_fmt.format(*[insert for c in args.commits]).strip()

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

        # TODO Is taking the average of all stats for the commit really what we want or is there a better metric?
        #      In addition, if the test environment is not set, are we combining tests from different environments?
        averageValuesOrNones = []
        for commit in args.commits:
            values = [float(t['value']) for t in metrics if t['commit'] == commit and t['test'] == test]
            if values == []:
                averageValuesOrNones.append(None)
            else:
                averageValuesOrNones.append(sum(values) / len(values))

        if flag == 'metrics':
            strings = [str(v) if v != None else '-' for v in averageValuesOrNones]
        if flag == 'percentages':
            # If the baseline commit has no stats, then we can not produce any percentages.
            baseline = averageValuesOrNones[0]
            if baseline == None:
                strings = ['-' for v in averageValuesOrNones]
            else:
                baseline = float(baseline)
                strings = ['-' if val == None else str(delta(baseline,float(val))) + '%' for val in averageValuesOrNones]

        return row_fmt.format(*strings).strip()

    #
    # The pretty-printed output
    #

    # TODO Sort the output by deviation. Most deviant tests are most important.
    #      Does the user want these at the bottom or top of the list?
    #      Color output?
    # TODO We currently only group on test name..... Shouldn't we group on ALL
    #      keys: 'test_env', 'test', 'way', 'metric', 'commit'

    header('commit')
    # Printing out metrics.
    all_tests = sorted(set([(stat['test'], stat['metric']) for stat in metrics]))
    for test, metric in all_tests:
        print("{:27}{:30}".format(test, metric) + commit_string(test,'metrics'))

    # Has no meaningful output if there is no commit to compare to.
    if not singleton_commit:
        header('percent')

        # Printing out percentages.
        for test, metric in all_tests:
            print("{:27}{:30}".format(test, metric) + commit_string(test,'percentages'))