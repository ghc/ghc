#!/usr/bin/env python3

#
# (c) Jared Weakly 2017
#
# This file will be a utility to help facilitate the comparison of performance
# metrics across arbitrary commits. The file will produce a table comparing
# metrics between measurements taken for given commits in the environment
# (which defaults to 'local' if not given by --test-env).
#

from enum import Enum
import colorsys
import tempfile
import json
import argparse
import re
import subprocess
import time
import sys

from collections import namedtuple, defaultdict
from math import ceil, trunc

from testutil import passed, failBecause, testing_metrics, print_table
from term_color import Color, colored

from my_typing import *

# Check if "git rev-parse" can be run successfully.
# True implies the current directory is a git repo.
_inside_git_repo_cache = None # type: Optional[bool]
def inside_git_repo() -> bool:
    global _inside_git_repo_cache
    if _inside_git_repo_cache is None:
        try:
            subprocess.check_call(['git', 'rev-parse', 'HEAD'],
                                stdout=subprocess.DEVNULL)
            _inside_git_repo_cache = True
        except subprocess.CalledProcessError:
            _inside_git_repo_cache = False
    return _inside_git_repo_cache

# Check if the worktree is dirty.
def is_worktree_dirty() -> bool:
    return subprocess.check_output(['git', 'status', '--porcelain']) != b''

# Get length of abbreviated git commit hash
def get_abbrev_hash_length() -> int:
    try:
        return len(subprocess.check_output(['git', 'rev-parse',
                                            '--short', 'HEAD']).strip())
    except subprocess.CalledProcessError:
        return 10

#
# Some data access functions. At the moment this uses git notes.
#

NoteNamespace = NewType("NoteNamespace", str)

# The git notes namespace for local results.
LocalNamespace = NoteNamespace("perf")

# The git notes namespace for ci results.
CiNamespace = NoteNamespace("ci/" + LocalNamespace)


# The metrics (a.k.a stats) are named tuples, PerfStat, in this form:
#
# ( test_env : 'val',      # Test environment.
#   test     : 'val',      # Name of the test
#   way      : 'val',
#   metric   : 'val',      # Metric being recorded
#   value    : 'val',      # The statistic result e.g. runtime
# )

# All the fields of a metric (excluding commit field).
PerfStat = NamedTuple('PerfStat', [('test_env', TestEnv),
                                   ('test', TestName),
                                   ('way', WayName),
                                   ('metric', MetricName),
                                   ('value', float)])

# A baseline recovered form stored metrics.
Baseline = NamedTuple('Baseline', [('perfStat', PerfStat),
                                   ('commit', GitHash)])

# The type of exceptions which are thrown when computing the current stat value
# fails.
class StatsException(Exception):
    pass

class MetricChange(Enum):
    # The metric appears to have no baseline and is presumably a new test.
    NewMetric = 'NewMetric'

    # The metric has not changed.
    NoChange = 'NoChange'

    # The metric increased.
    Increase = 'Increase'

    # The metric decreased.
    Decrease = 'Decrease'

    def __str__(self):
        strings = {
            MetricChange.NewMetric: colored(Color.BLUE,  "new"),
            MetricChange.NoChange:  colored(Color.WHITE, "unchanged"),
            MetricChange.Increase:  colored(Color.RED,   "increased"),
            MetricChange.Decrease:  colored(Color.GREEN, "decreased")
        }
        return strings[self]

    def hint(self):
        strings = {
            MetricChange.NewMetric: colored(Color.BLUE,"NEW"),
            MetricChange.NoChange:  "",
            MetricChange.Increase:  colored(Color.RED, "BAD"),
            MetricChange.Decrease:  colored(Color.GREEN,"GOOD")
        }
        return strings[self]

AllowedPerfChange = NamedTuple('AllowedPerfChange',
                               [('direction', MetricChange),
                                ('metrics', List[str]),
                                ('opts', Dict[str, str])
                                ])

class MetricAcceptanceWindow:
    """
    A strategy for computing an acceptance window for a metric measurement
    given a baseline value.
    """
    def get_bounds(self, baseline: float) -> Tuple[float, float]:
        raise NotImplemented
    def describe(self) -> str:
        raise NotImplemented

class AlwaysAccept(MetricAcceptanceWindow):
    def get_bounds(self, baseline: float) -> Tuple[float, float]:
        return (-1/0, +1/0)

    def describe(self) -> str:
        raise NotImplemented

class RelativeMetricAcceptanceWindow(MetricAcceptanceWindow):
    """
    A MetricAcceptanceWindow which accepts measurements within tol-percent of
    the baseline.
    """
    def __init__(self, tol: float):
        """ Accept any metric within tol-percent of the baseline """
        self.__tol = tol

    def get_bounds(self, baseline: float) -> Tuple[float, float]:
        lowerBound = trunc(           int(baseline) * ((100 - float(self.__tol))/100))
        upperBound = trunc(0.5 + ceil(int(baseline) * ((100 + float(self.__tol))/100)))

        return (lowerBound, upperBound)

    def describe(self) -> str:
        return '+/- %1.1f%%' % (100*self.__tol)

def parse_perf_stat(stat_str: str) -> PerfStat:
    field_vals = stat_str.strip('\t').split('\t')
    stat = PerfStat(*field_vals) # type: ignore
    if stat.test_env.startswith('"') and stat.test_env.endswith('"'):
        # Due to a bug, in historical data sometimes the test_env
        # contains additional quotation marks (#18656).
        # Remove them, so that we can refer to past data in a uniform fashion.
        stat = stat._replace(test_env=TestEnv(stat.test_env[1:-1]))
    return stat

# Get all recorded (in a git note) metrics for a given commit.
# Returns an empty array if the note is not found.
def get_perf_stats(commit: Union[GitRef, GitHash]=GitRef('HEAD'),
                   namespace: NoteNamespace = LocalNamespace
                   ) -> List[PerfStat]:
    try:
        log = subprocess.check_output(['git', 'notes', '--ref=' + namespace, 'show', commit], stderr=subprocess.STDOUT).decode('utf-8')
    except subprocess.CalledProcessError:
        return []

    return \
        [ parse_perf_stat(stat_str)
          for stat_str in log.strip('\n').split('\n')
          if stat_str != ''
        ]

# Check if a str is in a 40 character git commit hash.
_commit_hash_re = re.compile('[0-9a-f]' * 40)
def is_commit_hash(hash: str) -> bool:
    return _commit_hash_re.fullmatch(hash) != None

# Convert a <ref> to a commit hash code.
def commit_hash(commit: Union[GitHash, GitRef]) -> GitHash:
    if is_commit_hash(commit):
        return GitHash(commit)
    hash = subprocess.check_output(['git', 'rev-parse', commit], \
            stderr=subprocess.STDOUT) \
            .decode() \
            .strip()
    return GitHash(hash)

# Get allowed changes to performance. This is extracted from the commit message of
# the given commit in this form:
#     Metric  (Increase | Decrease)  ['metric' | \['metrics',..\]]  [\((test_env|way)='abc',...\)]: TestName01, TestName02, ...
# Returns a *dictionary* from test name to a *list* of items of the form:
#   {
#           'direction': either 'Increase' or 'Decrease,
#           'metrics': ['metricA', 'metricB', ...],
#           'opts': {
#                   'optionA': 'string value',
#                   'optionB': 'string value',          # e.g. test_env: "x86_64-linux"
#                   ...
#               }
#   }
_get_allowed_perf_changes_cache = {} # type: Dict[GitHash, Dict[TestName, List[AllowedPerfChange]]]
def get_allowed_perf_changes(commit: Union[GitRef, GitHash]=GitRef('HEAD')
                             ) -> Dict[TestName, List[AllowedPerfChange]]:
    global _get_allowed_perf_changes_cache
    chash = commit_hash(commit)
    if not chash in _get_allowed_perf_changes_cache:
        _get_allowed_perf_changes_cache[chash] \
            = parse_allowed_perf_changes(get_commit_message(chash))
    return _get_allowed_perf_changes_cache[chash]

# Get the commit message of any commit <ref>.
# This is cached (keyed on the full commit hash).
_get_commit_message = {} # type: Dict[GitHash, str]
def get_commit_message(commit: Union[GitHash, GitRef]=GitRef('HEAD')) -> str:
    global _get_commit_message
    commit = commit_hash(commit)
    if not commit in _get_commit_message:
        _get_commit_message[commit] = subprocess.check_output(\
            ['git', '--no-pager', 'log', '-n1', '--format=%B', commit]).decode()
    return _get_commit_message[commit]

def parse_allowed_perf_changes(commitMsg: str
                               ) -> Dict[TestName, List[AllowedPerfChange]]:
    # Helper regex. Non-capturing unless postfixed with Cap.
    s = r"(?:\s*\n?\s+)"                                    # Space, possible new line with an indent.
    qstr = r"(?:'(?:[^'\\]|\\.)*')"                         # Quoted string.
    qstrCap = r"(?:'((?:[^'\\]|\\.)*)')"                    # Quoted string. Captures the string without the quotes.
    innerQstrList = r"(?:"+qstr+r"(?:"+s+r"?,"+s+r"?"+qstr+r")*)?"     # Inside of a list of strings.gs.s..
    qstrList = r"(?:\["+s+r"?"+innerQstrList+s+r"?\])"      # A list of strings (using box brackets)..

    exp = (r"^\s*Metric"
        +s+r"(Increase|Decrease)"
        +s+r"?("+qstr+r"|"+qstrList+r")?"                   # Metric or list of metrics.s..
        +s+r"?(\(" + r"(?:[^')]|"+qstr+r")*" + r"\))?"      # Options surrounded in parenthesis. (allow parenthases in quoted strings)
        +s+r"?:?"                                           # Optional ":"
        +s+r"?((?:(?!\n\n)(?!\n[^\s])(?:.|\n))*)"           # Test names. Stop parsing on empty or non-indented new line.
        )

    matches = re.findall(exp, commitMsg, re.M)
    changes = {} # type: Dict[TestName, List[AllowedPerfChange]]
    for (direction, metrics_str, opts_str, tests_str) in matches:
        tests = tests_str.split()
        for test in tests:
            allowed = AllowedPerfChange(
                direction = MetricChange[direction],
                metrics = re.findall(qstrCap, metrics_str),
                opts = dict(re.findall(r"(\w+)"+s+r"?="+s+r"?"+qstrCap, opts_str))
            )
            changes.setdefault(test, []).append(allowed)

    return changes

# Calculates a suggested string to append to the git commit in order to accept the
# given changes.
# changes: [(MetricChange, PerfStat)]
def allow_changes_string(changes: List[Tuple[MetricChange, PerfStat]]
                         ) -> str:
    Dec = MetricChange.Decrease
    Inc = MetricChange.Increase

    # We only care about increase / decrease metrics.
    changes = [change for change in changes if change[0] in [Inc, Dec]]

    # Map tests to a map from change direction to metrics.
    test_to_dir_to_metrics = {} # type: Dict[TestName, Dict[MetricChange, List[MetricName]]]
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
        msgs.append('Metric Decrease:' + nltab + nltab.join(sorted(groupDec)))

    # Increasing group.
    if groupInc:
        msgs.append('Metric Increase:' + nltab + nltab.join(sorted(groupInc)))

    # Mixed group.
    if groupMix:
        # Split mixed group tests by decrease/increase, then by metric.
        dir_to_metric_to_tests = {
            Dec: {},
            Inc: {}
        } # type: Dict[MetricChange, Dict[MetricName, List[TestName]]]
        for test in groupMix:
            for change_dir, metrics in test_to_dir_to_metrics[test].items():
                for metric in metrics:
                    dir_to_metric_to_tests[change_dir].setdefault(metric, []).append(test)

        for change_dir in [Dec, Inc]:
            metric_to_tests = dir_to_metric_to_tests[change_dir]
            for metric in sorted(metric_to_tests.keys()):
                tests = sorted(metric_to_tests[metric])
                msgs.append('Metric ' + change_dir.value + ' \'' + metric + '\':' + nltab + nltab.join(tests))

    return '\n\n'.join(msgs)

# Formats a list of metrics into a string. Used e.g. to save metrics to a file or git note.
def format_perf_stat(stats: Union[PerfStat, List[PerfStat]], delimitor: str = "\t") -> str:
    # If a single stat, convert to a singleton list.
    if not isinstance(stats, list):
        stats = [stats]

    return "\n".join([delimitor.join([str(stat_val) for stat_val in stat]) for stat in stats])

# Appends a list of metrics to the git note of the given commit.
# Tries up to max_tries times to write to git notes should it fail for some reason.
# Each retry will wait 1 second.
# Returns True if the note was successfully appended.
def append_perf_stat(stats: List[PerfStat],
                     commit: GitRef = GitRef('HEAD'),
                     namespace: NoteNamespace = LocalNamespace,
                     max_tries: int=5
                     ) -> bool:
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

    print("\nAn error occurred while writing the performance metrics to git notes.\n \
            This is usually due to a lock-file existing somewhere in the git repo.")

    return False

#
# Baseline calculation
#

# Max number of ancestor commits to search when compiling a baseline performance metric.
BaselineSearchDepth = 1000

# (isCalculated, best fit ci test_env or None)
BestFitCiTestEnv = (False, None) # type: Tuple[bool, Optional[TestEnv]]

# test_env string or None
def best_fit_ci_test_env() -> Optional[TestEnv]:
    global BestFitCiTestEnv
    if not BestFitCiTestEnv[0]:
        platform = sys.platform
        isArch64 = sys.maxsize > 2**32
        arch = "x86_64" if isArch64 else "i386"

        if platform.startswith("linux"):
            test_env = TestEnv(arch + "-linux-deb9")  # type: Optional[TestEnv]
        elif platform.startswith("win32"):
            # There are no windows CI test results.
            test_env = None
        elif isArch64 and platform.startswith("darwin"):
            test_env = TestEnv(arch + "-darwin")
        elif isArch64 and platform.startswith("freebsd"):
            test_env = TestEnv(arch + "-freebsd")
        else:
            test_env = None

        BestFitCiTestEnv = (True, test_env)

    return BestFitCiTestEnv[1]

_baseline_depth_commit_log = {} # type: Dict[GitHash, List[GitHash]]

# Get the commit hashes for the last BaselineSearchDepth commits from and
# including the input commit. The output commits are all commit hashes.
def baseline_commit_log(commit: Union[GitHash,GitRef]) -> List[GitHash]:
    global _baseline_depth_commit_log
    chash = commit_hash(commit)
    if not commit in _baseline_depth_commit_log:
        _baseline_depth_commit_log[chash] = commit_log(chash, BaselineSearchDepth)

    return _baseline_depth_commit_log[chash]

# Get the commit hashes for the last n commits from and
# including the input commit. The output commits are all commit hashes.
# str -> [str]
def commit_log(commitOrRange, n=None):
    nArgs = ['-n' + str(n)] if n != None else []
    output = subprocess.check_output(['git', 'log', '--format=%H'] + nArgs + [commitOrRange]).decode()
    hashes = list(filter(is_commit_hash, output.split('\n')))

    # We only got 10 results (expecting 75) in a CI pipeline (issue #16662).
    # It's unclear from the logs what went wrong. Since no exception was
    # thrown, we can assume the `git log` call above succeeded. The best we
    # can do for now is improve logging.
    actualN = len(hashes)
    if n != None and actualN != n:
        print("Expected " + str(n) + " hashes, but git gave " + str(actualN) + ":\n" + output)
    return hashes

def add_new_changes(changes, new_changes):
   for key, new_change in new_changes.items():
       changes[key].extend(new_change)

def get_allowed_changes(baseline_ref: Optional[GitRef]) -> Dict[TestName, List[AllowedPerfChange]]:
 if baseline_ref:
        # The last 1000 commits in reverse order (starting from HEAD).
        commit_hashes = baseline_commit_log(GitRef("HEAD"))
        allowed_changes = defaultdict(list) # type: Dict[TestName, List[AllowedPerfChange]]
        for commit in commit_hashes:
            new_changes = get_allowed_perf_changes(commit)
            if commit == baseline_ref: return dict(allowed_changes)
            add_new_changes(allowed_changes, new_changes)
        print("PERF_BASELINE_COMMIT not found in last 1000 commits...")
        return dict()
 else:
        return get_allowed_perf_changes()

# Cache of baseline values. This is a dict of dicts indexed on:
# (useCiNamespace, commit) -> (test_env, test, metric, way) -> baseline
# (bool          , str   ) -> (str     , str , str   , str) -> float
_commit_metric_cache = {} # type: ignore

# Get the baseline of a test at a given commit. This is the expected value
# *before* the commit is applied (i.e. on the parent commit).
# This searches git notes from older commits for recorded metrics (locally and
# from ci). More recent commits are favoured, then local results over ci results
# are favoured.
#
# commit: str - must be a commit hash (see commit_hash())
# name: str - test name
# test_env: str - test environment (note a best fit test_env will be used
#                      instead when looking for ci results)
# metric: str - test metric
# way: str - test way
# returns: the Baseline or None if no metric was found within
#          BaselineSearchDepth commits and since the last expected change
#          (ignoring any expected change in the given commit).
def baseline_metric(commit: GitHash,
                    name: TestName,
                    test_env: TestEnv,
                    metric: MetricName,
                    way: WayName,
                    baseline_ref: Optional[GitRef]
                    ) -> Optional[Baseline]:
    # For performance reasons (in order to avoid calling commit_hash), we assert
    # commit is already a commit hash.
    assert is_commit_hash(commit)

    # Get all recent commit hashes.
    commit_hashes = baseline_commit_log(commit_hash(baseline_ref) if baseline_ref else commit)

    baseline_commit = None

    def has_expected_change(commit: GitHash) -> bool:
        return get_allowed_perf_changes(commit).get(name) is not None

    # Searches through previous commits trying local then ci for each commit in.
    def find_baseline(namespace: NoteNamespace,
                      test_env: TestEnv
                      ) -> Optional[Baseline]:
        if baseline_commit is not None:
            current_metric = get_commit_metric(namespace, baseline_commit, test_env, name, metric, way)
            if current_metric is not None:
                return Baseline(current_metric, baseline_commit)
            else:
                return None

        for depth, current_commit in list(enumerate(commit_hashes)):
            if current_commit == commit: continue
            # Check for a metric on this commit.
            current_metric = get_commit_metric(namespace, current_commit, test_env, name, metric, way)
            if current_metric is not None:
                return Baseline(current_metric, current_commit)

            # Stop if there is an expected change at this commit. In that case
            # metrics on ancestor commits will not be a valid baseline.
            if has_expected_change(current_commit):
                return None

        return None

    # Test environment to use when comparing against CI namespace
    ci_test_env = best_fit_ci_test_env()

    baseline = find_baseline(LocalNamespace, test_env) # type: Optional[Baseline]
    if baseline is None and ci_test_env is not None:
        baseline = find_baseline(CiNamespace, ci_test_env)

    return baseline

# Same as get_commit_metric(), but converts the result to a string or keeps it
# as None.
def get_commit_metric_value_str_or_none(gitNoteRef,
                                        commit: GitRef,
                                        test_env: TestEnv,
                                        name: TestName,
                                        metric: MetricName,
                                        way: WayName
                                        ) -> Optional[str]:
    result = get_commit_metric(gitNoteRef, commit, test_env, name, metric, way)
    if result is None:
        return None
    return str(result.value)

# gets the average commit metric from git notes.
# gitNoteRef: git notes ref space e.g. "perf" or "ci/perf"
# ref: git commit
# test_env: test environment
# name: test name
# metric: test metric
# way: test way
# returns: PerfStat | None if stats don't exist for the given input
def get_commit_metric(gitNoteRef,
                      ref: Union[GitRef, GitHash],
                      test_env: TestEnv,
                      name: TestName,
                      metric: MetricName,
                      way: WayName
                      ) -> Optional[PerfStat]:
    global _commit_metric_cache
    assert test_env != None
    commit = commit_hash(ref)

    # Check for cached value.
    cacheKeyA = (gitNoteRef, commit)
    cacheKeyB = (test_env, name, metric, way)
    if cacheKeyA in _commit_metric_cache:
        return _commit_metric_cache[cacheKeyA].get(cacheKeyB)

    # Cache miss.
    # Calculate baselines from the current commit's git note.
    # Note that the git note may contain data for other tests. All tests'
    # baselines will be collected and cached for future use.
    allCommitMetrics = get_perf_stats(ref, gitNoteRef)

    # Collect recorded values by cacheKeyB.
    values_by_cache_key_b = {}  # type: Dict[Tuple[TestEnv, TestName, MetricName, WayName], List[float]]
    for perfStat in allCommitMetrics:
        currentCacheKey = (perfStat.test_env, perfStat.test, \
                            perfStat.metric, perfStat.way)
        currentValues = values_by_cache_key_b.setdefault(currentCacheKey, [])
        currentValues.append(float(perfStat.value))

    # Calculate and baseline (average of values) by cacheKeyB.
    baseline_by_cache_key_b = {}
    for currentCacheKey, currentValues in values_by_cache_key_b.items():
        baseline_by_cache_key_b[currentCacheKey] = PerfStat( \
                currentCacheKey[0],
                currentCacheKey[1],
                currentCacheKey[3],
                currentCacheKey[2],
                sum(currentValues) / len(currentValues))

    # Save baselines to the cache.
    _commit_metric_cache[cacheKeyA] = baseline_by_cache_key_b
    return baseline_by_cache_key_b.get(cacheKeyB)

def check_stats_change(actual: PerfStat,
                       baseline: Baseline,
                       acceptance_window: MetricAcceptanceWindow,
                       allowed_perf_changes: Dict[TestName, List[AllowedPerfChange]] = {},
                       force_print = False
                       ) -> Tuple[MetricChange, Any]:
    """
    Check test stats. This prints the results for the user.

    Parameters:
    actual: the PerfStat with actual value
    baseline: the expected Baseline value (this should generally be derived
        from baseline_metric())
    acceptance_window: allowed deviation of the actual value from the expected
        value.
    allowed_perf_changes: allowed changes in stats. This is a dictionary as
        returned by get_allowed_perf_changes().
    force_print: Print stats even if the test stat was in the tolerance range.

    Returns a (MetricChange, pass/fail object) tuple. Passes if the stats are within the expected value ranges.
    """
    expected_val = baseline.perfStat.value
    full_name = actual.test + ' (' + actual.way + ')'


    lowerBound, upperBound = acceptance_window.get_bounds(expected_val)

    # Find the direction of change.
    change = MetricChange.NoChange
    if actual.value < lowerBound:
        change = MetricChange.Decrease
    elif actual.value > upperBound:
        change = MetricChange.Increase

    # Is the change allowed?
    allowed_change_directions =  [MetricChange.NoChange] + [ allow_stmt.direction
            for allow_stmt in allowed_perf_changes.get(actual.test, [])

            # List of metrics are not specified or the metric is in the list of metrics.
            if not allow_stmt.metrics or actual.metric in allow_stmt.metrics

            # way/test are not specified, or match the actual way/test.
            if ((not 'way'      in allow_stmt.opts.keys()) or actual.way      == allow_stmt.opts['way'])
            if ((not 'test_env' in allow_stmt.opts.keys()) or actual.test_env == allow_stmt.opts['test_env'])
        ]
    change_allowed = change in allowed_change_directions

    # Print errors and create pass/fail object.
    result = passed()
    if not change_allowed:
        error = str(change) + ' from ' + baseline.perfStat.test_env + \
                ' baseline @ %s' % baseline.commit
        print(actual.metric, error + ':')
        result = failBecause('stat ' + error, tag='stat')

    if not change_allowed or force_print:
        length = max(len(str(x)) for x in [expected_val, lowerBound, upperBound, actual.value])

        def display(descr, val, extra):
            print(descr, str(val).rjust(length), extra)

        display('    Expected    ' + full_name + ' ' + actual.metric + ':', expected_val, acceptance_window.describe())
        display('    Lower bound ' + full_name + ' ' + actual.metric + ':', lowerBound, '')
        display('    Upper bound ' + full_name + ' ' + actual.metric + ':', upperBound, '')
        display('    Actual      ' + full_name + ' ' + actual.metric + ':', actual.value, '')
        if actual.value != expected_val:
            if expected_val == 0:
                actual_dev = 100.0
            else:
                actual_dev = round(((float(actual.value) * 100)/ int(expected_val)) - 100, 1)
            display('    Deviation   ' + full_name + ' ' + actual.metric + ':', actual_dev, '%')

    return (change, result)

# Generate a css color (rgb) string based off of the hash of the input.
def hash_rgb_str(x) -> str:
    res = 10000.0
    rgb = colorsys.hsv_to_rgb((abs(int(hash(x))) % res)/res, 1.0, 0.9)
    return "rgb(" + str(int(rgb[0] * 255)) + ", " + str(int(rgb[1] * 255)) + ", " + str(int(rgb[2] * 255)) + ")"

def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--add-note", nargs=3,
                        help="Development only. --add-note N commit seed \
                        Adds N fake metrics to the given commit using the random seed.")
    parser.add_argument("--ci", action='store_true',
                        help="Use ci results. You must fetch these with:\n    " \
                            + "$ git fetch https://gitlab.haskell.org/ghc/ghc-performance-notes.git refs/notes/perf:refs/notes/ci/perf")

    group = parser.add_argument_group(title='Filtering', description="Select which subset of performance metrics to dump")
    group.add_argument("--test-env",
                       help="The given test environment to be compared. Use 'local' for locally run results. If using --ci, see .gitlab-ci file for TEST_ENV settings.")
    group.add_argument("--test-name",
                       help="Filters for tests matching the given regular expression.")
    group.add_argument("--metric",
                       help="Test metric (one of " + str(testing_metrics()) + ").")
    group.add_argument("--way",
                       help="Test way (for example, optasm).")

    group = parser.add_argument_group(title='Plotting', description="Plot historical performance metrics")
    group.add_argument("--chart", nargs='?', default=None, action='store', const='./PerformanceChart.html',
                       help='Create a chart of the results an save it to the given file. Default to "./PerformanceChart.html".')
    group.add_argument("--zero-y", action='store_true',
                       help='When charting, include 0 in y axis')

    parser.add_argument("commits", nargs='+',
                        help="Either a list of commits or a single commit range (e.g. HEAD~10..HEAD).")
    args = parser.parse_args()

    env = 'local'
    name = re.compile('.*')
    CommitAndStat = NamedTuple('CommitAndStat',
                               [('commit', GitHash), ('stat', PerfStat)])
    metrics = [] # type: List[CommitAndStat]
    singleton_commit = len(args.commits) == 1

    #
    # Main logic of the program when called from the command-line.
    #

    ref = NoteNamespace('perf')
    if args.ci:
        ref = NoteNamespace('ci/perf')

    commits = args.commits
    if args.commits:
        # Commit range
        if len(commits) == 1 and ".." in commits[0]:
            commits = list(reversed(commit_log(commits[0])))
        for c in commits:
            metrics += [CommitAndStat(c, stat) for stat in get_perf_stats(c, ref)]

    if args.metric:
        metrics = [test for test in metrics if test.stat.metric == args.metric]

    if args.way:
        metrics = [test for test in metrics if test.stat.way == args.way]

    if args.test_env:
        if '"' in args.test_env:
            raise Exception('test_env should not contain quotation marks')
        metrics = [test for test in metrics if test.stat.test_env == args.test_env]

    if args.test_name:
        nameRe = re.compile(args.test_name)
        metrics = [test for test in metrics if nameRe.search(test.stat.test)]

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
    # Chart
    #
    def metricAt(commit, testName, testMetric):
        values2 = [float(t.stat.value) for t in metrics if t.commit == commit \
                                                       and t.stat.test == testName \
                                                       and t.stat.metric == testMetric]
        if values2 == []:
            return None
        else:
            return (sum(values2) / len(values2))

    testSeries = list(set([(test.stat.test_env, test.stat.test, test.stat.metric, test.stat.way) for test in metrics]))

    #
    # Use Chart.js to visualize the data.
    #

    if args.chart:
        commitMsgs = dict([(h, get_commit_message(h)) for h in commits])
        chartData = {
                'type': 'line',
                'data': {
                    'labels': [commitMsgs[h].split("\n")[0] + " (" + \
                                    (h[:8] if is_commit_hash(h) else h) + \
                                ")" for h in commits],
                    'datasets': [{
                        'label': name + "(" + way + ") " + metric + " - " + env,
                        'data': [get_commit_metric_value_str_or_none(ref, commit, env, name, metric, way) \
                                        for commit in commits],

                        'fill': 'false',
                        'spanGaps': 'true',
                        'lineTension': 0,
                        'backgroundColor': hash_rgb_str((env, name, metric, way)),
                        'borderColor': hash_rgb_str((env, name, metric, way))
                    } for (env, name, metric, way) in testSeries]
                },
                'options': {
                    'scales': {
                        'yAxes': [{
                            'ticks': { 'beginAtZero': True }
                        }]
                    }
                }
            }


        # Try use local Chart.js file else use online version.
        tooltipjsFilePath = sys.path[0] + "/js/tooltip.js"
        chartjsFilePath = sys.path[0] + "/js/Chart-2.8.0.min.js"
        tooltipjsTag = None
        try:
            tooltipjsFile = open(tooltipjsFilePath, "r")
            tooltipjsTag = '<script>' + tooltipjsFile.read() + '</script>'
            tooltipjsFile.close()
        except:
            print("Failed to load custom tooltip: " + chartjsFilePath + ".")
            tooltipjsTag = None
        try:
            chartjsFile = open(chartjsFilePath, "r")
            chartjsTag = '<script>' + chartjsFile.read() + '</script>'
            chartjsFile.close()
        except:
            print("Failed to load " + chartjsFilePath + ", reverting to online Chart.js.")
            chartjsTag = '<script src="https://cdn.jsdelivr.net/npm/chart.js@2.8.0"></script>'

        file = open(args.chart, "w+t")
        print(\
            "<html>" + \
                '<head>\n' + \
                    (tooltipjsTag if tooltipjsTag is not None else '') + \
                    chartjsTag + \
                '</head>' + \
                '<body style="padding: 20px"><canvas id="myChart"></canvas><script>' + \
                    "var ctx = document.getElementById('myChart').getContext('2d');" + \
                    "var commitMsgs = " + json.dumps(commitMsgs, indent=2) + ";" + \
                    "var chartData = " + json.dumps(chartData, indent=2) + ";" + \
                    (("var chart = new Chart(ctx, setCustomTooltip(chartData, commitMsgs));") \
                        if tooltipjsTag is not None else \
                     ("var chart = new Chart(ctx, chartData);")) + \
                '</script></body>' + \
            "</html>"\
            , file=file)
        file.close()
        exit(0)

    #
    # Print the data in tablular format
    #

    #                  T1234                 T1234
    #              max_bytes             max_bytes
    #                 normal                normal
    # commit   x86_64-darwin       i386-linux-deb9
    # --------------------------------------------
    # HEAD              9123                  9123
    # HEAD~1           10023                 10023
    # HEAD~2           21234                 21234
    # HEAD~3           20000                 20000
    def strMetric(x):
        return '{:.2f}'.format(x.value) if x != None else ""
    # Data is in column major format, so transpose and pass to print_table.
    T = TypeVar('T')
    def transpose(xss: List[List[T]]) -> List[List[T]]:
        return list(map(list, zip(*xss)))

    headerCols = [ ["","","","Commit"] ] \
                + [ [name, metric, way, env] for (env, name, metric, way) in testSeries ]
    dataCols = [ commits ] \
                + [ [strMetric(get_commit_metric(ref, commit, env, name, metric, way)) \
                        for commit in commits ] \
                        for (env, name, metric, way) in testSeries ]
    print_table(transpose(headerCols), transpose(dataCols))

if __name__ == '__main__':
    main()
