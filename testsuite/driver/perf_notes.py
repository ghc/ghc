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

from collections import namedtuple
from math import ceil, trunc

from testutil import passed, failBecause


# Check if "git rev-parse" can be run successfully.
# True implies the current directory is a git repo.
def inside_git_repo():
    try:
        subprocess.check_call(['git', 'rev-parse', 'HEAD'],
                              stdout=subprocess.DEVNULL)
        return True
    except subprocess.CalledProcessError:
        return False

# Check if the worktree is dirty.
def is_worktree_dirty():
    return subprocess.check_output(['git', 'status', '--porcelain']) != b''

#
# Some data access functions. A the moment this uses git notes.
#

# The metrics (a.k.a stats) are named tuples, PerfStat, in this form:
#
# ( test_env : 'val',      # Test environment.
#   test     : 'val',      # Name of the test 
#   way      : 'val',
#   metric   : 'val',      # Metric being recorded
#   value    : 'val',      # The statistic result e.g. runtime
# )

# All the fields of a metric (excluding commit field).
PerfStat = namedtuple('PerfStat', ['test_env','test','way','metric','value'])

class MetricChange:
    NewMetric = 'NewMetric'
    NoChange = 'NoChange'
    Increase = 'Increase'
    Decrease = 'Decrease'

def parse_perf_stat(stat_str):
    field_vals = stat_str.strip('\t').split('\t')
    return PerfStat(*field_vals)

# Get all recorded (in a git note) metrics for a given commit.
# Returns an empty array if the note is not found.
def get_perf_stats(commit='HEAD', namespace='perf'):
    try:
        log = subprocess.check_output(['git', 'notes', '--ref=' + namespace, 'show', commit], stderr=subprocess.STDOUT).decode('utf-8')
    except subprocess.CalledProcessError:
        return []

    log = log.strip('\n').split('\n')
    log = list(filter(None, log))
    log = [parse_perf_stat(stat_str) for stat_str in log]
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
    for (direction, metrics_str, opts_str, tests_str) in matches:
        tests = re.findall(r"(\w+)", tests_str)
        for test in tests:
            changes.setdefault(test, []).append({
                'direction': direction,
                'metrics': re.findall(qstrCap, metrics_str),
                'opts': dict(re.findall(r"(\w+)"+s+r"?="+s+r"?"+qstrCap, opts_str))
            })

    return changes

# Calculates a suggested string to append to the git commit in order to accept the
# given changes.
# changes: [(MetricChange, PerfStat)]
def allow_changes_string(changes):
    Dec = MetricChange.Decrease
    Inc = MetricChange.Increase

    # We only care about increase / decrease metrics.
    changes = [change for change in changes if change[0] in [Inc, Dec]]

    # Map tests to a map from change direction to metrics.
    test_to_dir_to_metrics = {}
    for (change, perf_stat) in changes:
        change_dir_to_metrics = test_to_dir_to_metrics.setdefault(perf_stat.test, { Inc: [], Dec: [] })
        change_dir_to_metrics[change].append(perf_stat.metric)

    # Split into 3 groups.
    # Tests where all changes are *increasing*.
    # Tests where all changes are *decreasing*.
    # Tests where changes are *mixed* increasing and decreasing.
    groupDec = []
    groupInc = []
    groupMix = []
    for (test, decsAndIncs) in test_to_dir_to_metrics.items():
        decs = decsAndIncs[Dec]
        incs = decsAndIncs[Inc]
        if decs and incs:
            groupMix.append(test)
        elif not decs:
            groupInc.append(test)
        else:
            groupDec.append(test)

    msgs = []
    nltab = '\n    '

    # Decreasing group.
    if groupDec:
        msgs.append('Metric Decrease:' + nltab + nltab.join(groupDec))

    # Increasing group.
    if groupInc:
        msgs.append('Metric Increase:' + nltab + nltab.join(groupInc))

    # Mixed group.
    if groupMix:
        # Split mixed group tests by decrease/increase, then by metric.
        dir_to_metric_to_tests = {
                Dec: {},
                Inc: {}
            }
        for test in groupMix:
            for change_dir, metrics in test_to_dir_to_metrics[test].items():
                for metric in metrics:
                    dir_to_metric_to_tests[change_dir].setdefault(metric, []).append(test)

        for change_dir in [Dec, Inc]:
            metric_to_tests = dir_to_metric_to_tests[change_dir]
            for metric in sorted(metric_to_tests.keys()):
                tests = metric_to_tests[metric]
                msgs.append('Metric ' + change_dir + ' \'' + metric + '\':' + nltab + nltab.join(tests))

    return '\n\n'.join(msgs)

# Formats a list of metrics into a string. Used e.g. to save metrics to a file or git note.
def format_perf_stat(stats):
    # If a single stat, convert to a singleton list.
    if not isinstance(stats, list):
        stats = [stats]

    return "\n".join(["\t".join([str(stat_val) for stat_val in stat]) for stat in stats])

# Appends a list of metrics to the git note of the given commit.
# Tries up to max_tries times to write to git notes should it fail for some reason.
# Each retry will wait 1 second.
# Returns True if the note was successfully appended.
def append_perf_stat(stats, commit='HEAD', namespace='perf', max_tries=5):
    # Append to git note
    print('Appending ' + str(len(stats)) + ' stats to git notes.')
    stats_str = format_perf_stat(stats)
    def try_append():
            try:
                return subprocess.check_output(['git', 'notes', '--ref=' + namespace, 'append', commit, '-m', stats_str])
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
# actual: the PerfStat with actual value.
# expected_val: the expected value (this should generally be derived from get_perf_stats())
# tolerance_dev: allowed deviation of the actual value from the expected value.
# allowed_perf_changes: allowed changes in stats. This is a dictionary as returned by get_allowed_perf_changes().
# force_print: Print stats even if the test stat was in the tolerance range.
# Returns a (MetricChange, pass/fail object) tuple. Passes if the stats are withing the expected value ranges.
def check_stats_change(actual, expected_val, tolerance_dev, allowed_perf_changes = {}, force_print = False):
    full_name = actual.test + ' (' + actual.way + ')'

    lowerBound = trunc(           int(expected_val) * ((100 - float(tolerance_dev))/100))
    upperBound = trunc(0.5 + ceil(int(expected_val) * ((100 + float(tolerance_dev))/100)))

    actual_dev = round(((float(actual.value) * 100)/ int(expected_val)) - 100, 1)

    # Find the direction of change.
    change = MetricChange.NoChange
    if actual.value < lowerBound:
        change = MetricChange.Decrease
    elif actual.value > upperBound:
        change = MetricChange.Increase

    # Is the change allowed?
    allowed_change_directions =  [MetricChange.NoChange] + [ allow_stmt['direction']
            for allow_stmt in allowed_perf_changes.get(actual.test, [])

            # List of metrics are not specified or the metric is in the list of metrics.
            if not allow_stmt['metrics'] or actual.metric in allow_stmt['metrics']

            # way/test are not specified, or match the actual way/test.
            if ((not 'way'      in allow_stmt['opts'].keys()) or actual.way      == allow_stmt['opts']['way'])
            if ((not 'test_env' in allow_stmt['opts'].keys()) or actual.test_env == allow_stmt['opts']['test_env'])
        ]
    change_allowed = change in allowed_change_directions

    # Print errors and create pass/fail object.
    result = passed()
    if not change_allowed:
        error = change + ' not allowed'
        print(actual.metric, error + ':')
        result = failBecause('stat ' + error, tag='stat')

    if not change_allowed or force_print:
        length = max(len(str(x)) for x in [expected_val, lowerBound, upperBound, actual.value])

        def display(descr, val, extra):
            print(descr, str(val).rjust(length), extra)

        display('    Expected    ' + full_name + ' ' + actual.metric + ':', expected_val, '+/-' + str(tolerance_dev) + '%')
        display('    Lower bound ' + full_name + ' ' + actual.metric + ':', lowerBound, '')
        display('    Upper bound ' + full_name + ' ' + actual.metric + ':', upperBound, '')
        display('    Actual      ' + full_name + ' ' + actual.metric + ':', actual.value, '')
        if actual.value != expected_val:
            display('    Deviation   ' + full_name + ' ' + actual.metric + ':', actual_dev, '%')

    return (change, result)

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
    # metrics is a tuple (str commit, PerfStat stat)
    CommitAndStat = namedtuple('CommitAndStat', ['commit', 'stat'])
    metrics = []
    singleton_commit = len(args.commits) == 1

    #
    # Main logic of the program when called from the command-line.
    #

    if args.commits:
        for c in args.commits:
            metrics += [CommitAndStat(c, stat) for stat in get_perf_stats(c)]

    if args.test_env:
        metrics = [test for test in metrics if test.stat.test_env == args.test_env]

    if args.test_name:
        nameRe = re.compile(args.test_name)
        metrics = [test for test in metrics if nameRe.search(test.test)]

    if args.add_note:
        def note_gen(n, commit, delta=''):
            note = []
            # Generates simple fake data. Likely not comprehensive enough to catch all edge cases.
            if not delta:
                note.extend([PerfStat('local', 'T'+ str(i*100), 'some_way', 'some_field', str(i*1000)) for i in range(1,int(int(n)/2)+1)])
                note.extend([PerfStat('non-local', 'W'+ str(i*100), 'other_way', 'other_field', str(i*100)) for i in range(int(int(n)/2)+1,int(n)+1)])
            if delta:
                hu = abs(hash(delta))
                hv = abs(hash(hu))
                u = int(hu % 100)
                v = int(hv % 10)
                note.extend([PerfStat('local', 'T'+ str(i*100), 'some_way', 'some_field', str(i*u)) for i in range(1,int(int(n)/2)+1)])
                note.extend([PerfStat('non-local', 'W'+ str(i*100), 'other_way', 'other_field', str(i*v)) for i in range(int(int(n)/2)+1,int(n)+1)])

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

        # Get the average value per commit (or None if that commit contains no metrics).
        # Note: if the test environment is not set, this will combine metrics from all test environments.
        averageValuesOrNones = []
        for commit in args.commits:
            values = [float(t.stat.value) for t in metrics if t.commit == commit and t.stat.test == test]
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

    header('commit')
    # Printing out metrics.
    all_tests = sorted(set([(test.stat.test, test.stat.metric) for test in metrics]))
    for test, metric in all_tests:
        print("{:27}{:30}".format(test, metric) + commit_string(test,'metrics'))

    # Has no meaningful output if there is no commit to compare to.
    if not singleton_commit:
        header('percent')

        # Printing out percentages.
        for test, metric in all_tests:
            print("{:27}{:30}".format(test, metric) + commit_string(test,'percentages'))
