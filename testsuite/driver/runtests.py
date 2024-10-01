#!/usr/bin/env python3

#
# (c) Simon Marlow 2002
#

import argparse
import signal
import sys
import os
import io
import operator
import shutil
import tempfile
import time
import re
import traceback
from functools import reduce
from pathlib import Path

# We don't actually need subprocess in runtests.py, but:
# * We do need it in testlibs.py
# * We can't import testlibs.py until after we have imported ctypes
# * If we import ctypes before subprocess on cygwin, then sys.exit(0)
#   says "Aborted" and we fail with exit code 134.
# So we import it here first, so that the testsuite doesn't appear to fail.
import subprocess

import asyncio

from testutil import getStdout, str_warn, str_info, print_table, shorten_metric_name
from testglobals import getConfig, ghc_env, TestConfig, t, \
                        TestOptions, brokens, PerfMetric
from my_typing import TestName
from perf_notes import MetricChange, GitRef, inside_git_repo, is_worktree_dirty, format_perf_stat, get_abbrev_hash_length, is_commit_hash
from junit import junit
import term_color
from term_color import Color, colored
import cpu_features

# Readline sometimes spews out ANSI escapes for some values of TERM,
# which result in test failures. Thus set TERM to a nice, simple, safe
# value.
os.environ['TERM'] = 'vt100'
ghc_env['TERM'] = 'vt100'

# Ensure that GHC doesn't go looking for environment files. See #21365.
ghc_env['GHC_ENVIRONMENT'] = "-"

# Ensure that EMCC doesn't output cache info
# (cf https://github.com/emscripten-core/emscripten/issues/18607)
os.environ['EMCC_LOGGING'] = '0'
ghc_env['EMCC_LOGGING'] = '0'

global config
config = getConfig() # get it from testglobals
config.validate()

def signal_handler(signal, frame):
    stopNow()

def get_compiler_info() -> TestConfig:
    """ Overriddden by configuration file. """
    raise NotImplementedError

# -----------------------------------------------------------------------------
# cmd-line options

parser = argparse.ArgumentParser(description="GHC's testsuite driver")
perf_group = parser.add_mutually_exclusive_group()

parser.add_argument("-e", action='append', help="A string to execute from the command line.")
parser.add_argument("--top", type=Path, help="path to top of testsuite/ tree")
parser.add_argument("--config-file", action="append", help="config file")
parser.add_argument("--config", action='append', help="config field")
parser.add_argument("--rootdir", action='append', help="root of tree containing tests (default: .)")
parser.add_argument("--metrics-file", help="file in which to save (append) the performance test metrics. If omitted, git notes will be used.")
parser.add_argument("--summary-file", help="file in which to save the (human-readable) summary")
parser.add_argument("--unexpected-output-dir", help="directory in which to place unexpected output")
parser.add_argument("--target-wrapper", help="wrapper executable to use when executing binaries compiled for the target")
parser.add_argument("--only", action="append", help="just this test (can be give multiple --only= flags)")
parser.add_argument("--way", action="append", help="just this way")
parser.add_argument("--skipway", action="append", help="skip this way")
parser.add_argument("--threads", type=int, help="threads to run simultaneously")
parser.add_argument("--verbose", type=int, choices=[0,1,2,3,4,5], help="verbose (Values 0 through 5 accepted)")
parser.add_argument("--junit", type=argparse.FileType('wb'), help="output testsuite summary in JUnit format")
parser.add_argument("--broken-test", action="append", default=[], help="a test name to mark as broken for this run")
parser.add_argument("--test-env", default='local', help="Override default chosen test-env.")
parser.add_argument("--perf-baseline", type=GitRef, metavar='COMMIT', help="Baseline commit for performance comparsons.")
perf_group.add_argument("--skip-perf-tests", action="store_true", help="skip performance tests")
perf_group.add_argument("--only-perf-tests", action="store_true", help="Only do performance tests")
parser.add_argument("--ignore-perf-failures", choices=['increases','decreases','all'],
                        help="Do not fail due to out-of-tolerance perf tests")
parser.add_argument("--only-report-hadrian-deps", type=argparse.FileType('w'),
                        help="Dry run the testsuite and report all extra hadrian dependencies needed on the given file")

args = parser.parse_args()

# Initialize variables that are set by the build system with -e
windows = False
darwin = False

if args.e:
    for e in args.e:
        exec(e)

if args.config_file:
    for arg in args.config_file:
        with io.open(arg) as f:
            exec(f.read())

if args.config:
    for arg in args.config:
        field, value = arg.split('=', 1)
        setattr(config, field, value)

all_ways = config.run_ways+config.compile_ways+config.other_ways

if args.rootdir:
    config.rootdirs = args.rootdir

config.metrics_file = args.metrics_file
hasMetricsFile = config.metrics_file is not None
config.summary_file = args.summary_file
config.baseline_commit = args.perf_baseline
config.target_wrapper = args.target_wrapper

if args.top:
    config.top = args.top

if args.unexpected_output_dir:
    config.unexpected_output_dir = Path(args.unexpected_output_dir)

if args.only:
    config.only = args.only
    config.run_only_some_tests = True

if args.way:
    for way in args.way:
        if way not in all_ways:
            print('WARNING: Unknown WAY %s in --way' % way)
        else:
            config.cmdline_ways += [way]
            if way in config.other_ways:
                config.run_ways += [way]
                config.compile_ways += [way]

if args.skipway:
    for way in args.skipway:
        if way not in all_ways:
            print('WARNING: Unknown WAY %s in --skipway' % way)

    config.other_ways = [w for w in config.other_ways if w not in args.skipway]
    config.run_ways = [w for w in config.run_ways if w not in args.skipway]
    config.compile_ways = [w for w in config.compile_ways if w not in args.skipway]

config.broken_tests |= {TestName(t) for t in args.broken_test}

if args.threads:
    config.threads = args.threads

if args.verbose is not None:
    config.verbose = args.verbose

config.only_report_hadrian_deps = args.only_report_hadrian_deps


# Note force skip perf tests: skip if this is not a git repo (estimated with inside_git_repo)
# and no metrics file is given. In this case there is no way to read the previous commit's
# perf test results, nor a way to store new perf test results.
forceSkipPerfTests = not hasMetricsFile and not inside_git_repo()
config.skip_perf_tests = args.skip_perf_tests or forceSkipPerfTests
config.only_perf_tests = args.only_perf_tests
if args.ignore_perf_failures == 'all':
    config.ignore_perf_decreases = True
    config.ignore_perf_increases = True
elif args.ignore_perf_failures == 'increases':
    config.ignore_perf_increases = True
elif args.ignore_perf_failures == 'decreases':
    config.ignore_perf_decreases = True

if args.test_env:
    config.test_env = args.test_env

config.cygwin = False
config.msys = False

if windows:
    h = os.popen('uname -s', 'r')
    v = h.read()
    h.close()
    if v.startswith("CYGWIN"):
        config.cygwin = True
    elif v.startswith("MINGW") or v.startswith("MSYS"):
# msys gives "MINGW32"
# msys2 gives "MINGW_NT-6.2" or "MSYS_NT-6.3"
        config.msys = True
    else:
        raise Exception("Can't detect Windows terminal type")

# Try to use UTF8
if windows:
    import ctypes
    # Windows and mingw* Python provide windll, msys2 python provides cdll.
    if hasattr(ctypes, 'WinDLL'):
        mydll = ctypes.WinDLL    # type: ignore
    else:
        mydll = ctypes.CDLL

    # This actually leaves the terminal in codepage 65001 (UTF8) even
    # after python terminates. We ought really remember the old codepage
    # and set it back.
    kernel32 = mydll('kernel32.dll')
    if kernel32.SetConsoleCP(65001) == 0:
        raise Exception("Failure calling SetConsoleCP(65001)")
    if kernel32.SetConsoleOutputCP(65001) == 0:
        raise Exception("Failure calling SetConsoleOutputCP(65001)")

    # register the interrupt handler
    signal.signal(signal.SIGINT, signal_handler)
else:
    # Try and find a utf8 locale to use
    # First see if we already have a UTF8 locale
    h = os.popen('locale | grep LC_CTYPE | grep -i utf', 'r')
    v = h.read()
    h.close()
    if v == '':
        # We don't, so now see if 'locale -a' works
        h = os.popen('locale -a | grep -F .', 'r')
        v = h.read()
        h.close()
        if v != '':
            # If it does then use the first utf8 locale that is available
            h = os.popen(r'locale -a | grep -i "utf8\|utf-8" 2>/dev/null', 'r')
            v = h.readline().strip()
            h.close()
            if v != '':
                os.environ['LC_ALL'] = v
                ghc_env['LC_ALL'] = v
                print("setting LC_ALL to", v)
            else:
                print('WARNING: No UTF8 locale found.')
                print('You may get some spurious test failures.')

# https://stackoverflow.com/a/22254892/1308058
def supports_colors():
    """
    Returns True if the running system's terminal supports color, and False
    otherwise.
    """
    plat = sys.platform
    supported_platform = plat != 'Pocket PC' and (plat != 'win32' or
                                                  'ANSICON' in os.environ)
    # isatty is not always implemented, #6223.
    is_a_tty = hasattr(sys.stdout, 'isatty') and sys.stdout.isatty()
    if not supported_platform or not is_a_tty:
        return False
    return True

config.supports_colors = supports_colors()
term_color.enable_color = config.supports_colors

# This has to come after arg parsing as the args can change the compiler
get_compiler_info()

# Can't import this earlier as we need to know if threading will be
# enabled or not
from testlib import *

def format_path(path):
    if windows:
        if os.pathsep == ':':
            # If using msys2 python instead of mingw we have to change the drive
            # letter representation. Otherwise it thinks we're adding two env
            # variables E and /Foo when we add E:/Foo.
            path = re.sub('([a-zA-Z]):', '/\\1', path)
        if config.cygwin:
            # On cygwin we can't put "c:\foo" in $PATH, as : is a
            # field separator. So convert to /cygdrive/c/foo instead.
            # Other pythons use ; as the separator, so no problem.
            path = re.sub('([a-zA-Z]):', '/cygdrive/\\1', path)
            path = re.sub('\\\\', '/', path)
    return path

# On Windows we need to set $PATH to include the paths to all the DLLs
# in order for the dynamic library tests to work.
if windows:
    try:
        pkginfo = getStdout([config.ghc_pkg, 'dump'])
    except FileNotFoundError as err:
        # This can happen when we are only running tests which don't depend on ghc (ie linters)
        # In that case we probably haven't built ghc-pkg yet so this query will fail.
        print (err)
        print ("Failed to call ghc-pkg for windows path modification... some tests might fail")
        pkginfo = ""
    topdir = config.libdir
    mingw = os.path.abspath(os.path.join(topdir, '../mingw/bin'))
    mingw = format_path(mingw)
    ghc_env['PATH'] = os.pathsep.join([ghc_env.get("PATH", ""), mingw])
    for line in pkginfo.split('\n'):
        if line.startswith('library-dirs:'):
            path = line.rstrip()
            path = re.sub('^library-dirs: ', '', path)
            # Use string.replace instead of re.sub, because re.sub
            # interprets backslashes in the replacement string as
            # escape sequences.
            path = path.replace('$topdir', topdir)
            if path.startswith('"'):
                path = re.sub('^"(.*)"$', '\\1', path)
                path = re.sub('\\\\(.)', '\\1', path)

                path = format_path(path)
                ghc_env['PATH'] = os.pathsep.join([path, ghc_env.get("PATH", "")])

testopts_ctx_var.set(TestOptions())

# if timeout == -1 then we try to calculate a sensible value
if config.timeout == -1:
    config.timeout = int(read_no_crs(config.top / 'timeout' / 'calibrate.out'))

print('Timeout is ' + str(config.timeout))
print('Known ways: ' + ', '.join(config.other_ways))
print('Run ways: ' + ', '.join(config.run_ways))
print('Compile ways: ' + ', '.join(config.compile_ways))

# Try get allowed performance changes from the git commit.
try:
    config.allowed_perf_changes = Perf.get_allowed_changes(config.baseline_commit)
except subprocess.CalledProcessError:
    print('Failed to get allowed metric changes from the HEAD git commit message.')


print('Allowing performance changes in: ' + ', '.join(config.allowed_perf_changes.keys()))

# -----------------------------------------------------------------------------
# The main dude

if config.rootdirs == []:
    config.rootdirs = ['.']

t_files = list(findTFiles(config.rootdirs))

print('Found', len(t_files), '.T files...')

# Avoid cmd.exe built-in 'date' command on Windows
t.start_time = datetime.datetime.now()

print('Beginning test run at', t.start_time.strftime("%c %Z"))

# For reference
try:
    print('Detected CPU features: ', cpu_features.get_cpu_features())
except Exception as e:
    print('Failed to detect CPU features: ', e)

sys.stdout.flush()

if config.local:
    tempdir = ''
else:
    # See note [Running tests in /tmp]
    tempdir = tempfile.mkdtemp('', 'ghctest-')

    # opts.testdir should be quoted when used, to make sure the testsuite
    # keeps working when it contains backward slashes, for example from
    # using os.path.join. Windows native and mingw* python
    # (/mingw64/bin/python) set `os.path.sep = '\\'`, while msys2 python
    # (/bin/python, /usr/bin/python or /usr/local/bin/python) sets
    # `os.path.sep = '/'`.
    # To catch usage of unquoted opts.testdir early, insert some spaces into
    # tempdir.
    tempdir = os.path.join(tempdir, 'test   spaces')

def cleanup_and_exit(exitcode):
    if config.cleanup and tempdir:
        shutil.rmtree(tempdir, ignore_errors=True)
    exit(exitcode)

def geometric_mean(xs):
    if len(xs) > 0:
      return reduce(operator.mul, xs)**(1. / len(xs))
    else:
      return 1

def tabulate_metrics(metrics: List[PerfMetric]) -> None:
    abbrevLen = get_abbrev_hash_length()
    hasBaseline = any([x.baseline is not None for x in metrics])
    baselineCommitSet = set([x.baseline.commit for x in metrics if x.baseline is not None])
    hideBaselineCommit = not hasBaseline or len(baselineCommitSet) == 1
    hideBaselineEnv = not hasBaseline or all(
        [x.stat.test_env == x.baseline.perfStat.test_env
         for x in metrics if x.baseline is not None])
    def row(cells: Tuple[str, str, str, str, str, str, str, str]) -> List[str]:
        return [x for (idx, x) in enumerate(list(cells)) if
                (idx != 2 or not hideBaselineCommit) and
                (idx != 3 or not hideBaselineEnv )]

    headerRows = [
        row(("", "", "Baseline", "Baseline", "Baseline", "", "", "")),
        row(("Test", "Metric", "commit", "environment", "value", "New value", "Change", ""))
    ]
    def strDiff(x: PerfMetric) -> str:
        if x.baseline is None:
            return ""
        val0 = x.baseline.perfStat.value
        val1 = x.stat.value
        if val0 == 0 and val1 == 0:
            return "0.0%"
        elif val0 == 0:
            return "NaN%"
        else:
            return "{:+2.1f}%".format(100 * (val1 - val0) / val0)
    dataRows = [row((
        "{}({})".format(x.stat.test, x.stat.way),
        shorten_metric_name(x.stat.metric),
          "{}".format(x.baseline.commit[:abbrevLen]
                      if is_commit_hash(x.baseline.commit) else x.baseline.commit)
          if x.baseline is not None else "",
        "{}".format(x.baseline.perfStat.test_env)
          if x.baseline is not None else "",
        "{:13,d}".format(int(x.baseline.perfStat.value))
          if x.baseline is not None else "",
        "{:13,d}".format(int(x.stat.value)),
        strDiff(x),
        "{}".format(x.change.hint())
    )) for x in sorted(metrics, key =
                      lambda m: (m.stat.test, m.stat.way, m.stat.metric))]

    changes = [
        x.stat.value / x.baseline.perfStat.value
        for x in metrics
        if x.baseline is not None
        if x.baseline.perfStat.value != 0
    ]
    minimum = 0.0
    maximum = 0.0
    if len(changes) > 0:
      minimum = 100 * (min(changes) - 1)
      maximum = 100 * (max(changes) - 1)
    dataRows += [
        row(("", "", "", "", "", "", "", "")),
        row(("geo. mean", "", "", "", "", "", "{:+4.1f}%".format(100*(geometric_mean(changes)-1)), "")),
        row(("minimum  ", "", "", "", "", "", "{:+4.1f}%".format(minimum), "")),
        row(("maximum  ", "", "", "", "", "", "{:+4.1f}%".format(maximum), "")),
    ]
    print_table(headerRows, dataRows, 1)
    print("")
    if hasBaseline:
        if hideBaselineEnv:
            print("* All baselines were measured in the same environment as this test run")
        if hideBaselineCommit:
            commit = next(iter(baselineCommitSet))
            print("* All baseline commits are {}".format(
                commit[:abbrevLen]
                if is_commit_hash(commit) else commit
            ))

# First collect all the tests to be run
t_files_ok = True
for file in t_files:
    if_verbose(3, '====> Scanning %s' % file)
    newTestDir(tempdir, os.path.dirname(file))
    try:
        with io.open(file, encoding='utf8') as f:
            src = f.read()

        exec(src)
    except Exception as e:
        traceback.print_exc()
        framework_fail(None, None, 'exception: %s' % e)
        t_files_ok = False

for name in config.only:
    if t_files_ok:
        # See Note [Mutating config.only]
        framework_fail(name, None, 'test not found')
    else:
        # Let user fix .T file errors before reporting on unfound tests.
        # The reason the test can not be found is likely because of those
        # .T file errors.
        pass

if config.list_broken:
    print('')
    print('Broken tests:')
    print('\n  '.join('#{ticket}({a}/{b})'.format(ticket=ticket, a=a, b=b)
                      for ticket, a, b in brokens))
    print('')

    if t.framework_failures:
        print('WARNING:', len(t.framework_failures), 'framework failures!')
        print('')
else:
    # Now run all the tests
    try:
        async def run_parallelTests():
            sem = asyncio.Semaphore(config.threads)
            ts = []

            for oneTest in parallelTests:
                if stopping():
                    break
                ts.append(oneTest(sem))

            return await asyncio.gather(*ts)

        # wait for parallel tests to finish
        asyncio.run(run_parallelTests())

        # Run the following tests purely sequential
        async def run_aloneTests():
            for oneTest in aloneTests:
                if stopping():
                    break
                await oneTest(None)

        asyncio.run(run_aloneTests())

    except KeyboardInterrupt:
        pass

    # flush everything before we continue
    sys.stdout.flush()

    # Dump metrics data.
    print("\nPerformance Metrics (test environment: {}):\n".format(config.test_env))
    if config.baseline_commit:
        print('Performance baseline: %s\n' % config.baseline_commit)
    if any(t.metrics):
        # Group metrics by metric type
        groups = {} # type: Dict[MetricName, List[PerfMetric]]
        for m in t.metrics:
            if m.stat.metric not in groups:
                groups[m.stat.metric] = []

            groups[m.stat.metric].append(m)

        for metric_name, stats in groups.items(): # type: ignore
            heading = 'Metrics: %s' % metric_name
            print()
            print(heading)
            print('-' * len(heading))
            print()
            tabulate_metrics(stats) # type: ignore
    else:
        print("\nNone collected.")
    print("")

    # Warn if had to force skip perf tests (see Note force skip perf tests).
    spacing = "       "
    if forceSkipPerfTests and not args.skip_perf_tests:
        print()
        print(str_warn('Skipping All Performance Tests') + ' `git` exited with non-zero exit code.')
        print(spacing + 'Git is required because performance test results are compared with ancestor git commits\' results (stored with git notes).')
        print(spacing + 'You can still run the tests without git by specifying an output file with --metrics-file FILE.')

    # Warn of new metrics.
    new_metrics = [metric for (change, metric, baseline) in t.metrics if change == MetricChange.NewMetric]
    if any(new_metrics):
        if inside_git_repo():
            reason = 'a baseline (expected value) cannot be recovered from' + \
                ' previous git commits. This may be due to HEAD having' + \
                ' new tests or having expected changes, the presence of' + \
                ' expected changes since the last run of the tests, and/or' + \
                ' the latest test run being too old.'
            fix = 'If the tests exist on the previous' + \
                ' commit (And are configured to run with the same ways),' + \
                ' then check out that commit and run the tests to generate' + \
                ' the missing metrics. Alternatively, a baseline may be' + \
                ' recovered from ci results once fetched:\n\n' + \
                spacing + 'git fetch ' + \
                  'https://gitlab.haskell.org/ghc/ghc-performance-notes.git' + \
                  ' refs/notes/perf:refs/notes/' + Perf.CiNamespace
        else:
            reason = "this is not a git repo so the previous git commit's" + \
                     " metrics cannot be loaded from git notes:"
            fix = ""
        print()
        print(str_warn('Missing Baseline Metrics') + \
                ' these metrics trivially pass because ' + reason)
        print(spacing + (' ').join(set([metric.test for metric in new_metrics])))
        if fix != "":
            print()
            print(fix)

    # Inform of how to accept metric changes.
    if (len(t.unexpected_stat_failures) > 0):
        print()
        print(str_info("Some stats have changed") + " If this is expected, " + \
            "allow changes by appending the git commit message with this:")
        print('-' * 25)
        print(Perf.allow_changes_string([(m.change, m.stat) for m in t.metrics]))
        print('-' * 25)

    summary(t, sys.stdout, color=config.supports_colors)

    # Write perf stats if any exist or if a metrics file is specified.
    stats_metrics = [stat for (_, stat, __) in t.metrics] # type: List[PerfStat]
    if hasMetricsFile:
        print('Appending ' + str(len(stats_metrics)) + ' stats to file: ' + config.metrics_file)
        with open(config.metrics_file, 'a') as f:
            f.write("\n" + Perf.format_perf_stat(stats_metrics))
    elif inside_git_repo() and any(stats_metrics):
        if is_worktree_dirty():
            print()
            print(str_warn('Performance Metrics NOT Saved') + \
                ' working tree is dirty. Commit changes or use ' + \
                '--metrics-file to save metrics to a file.')
        else:
            Perf.append_perf_stat(stats_metrics)

    # Write summary
    if config.summary_file:
        with open(config.summary_file, 'w') as f:
            summary(t, f)

    if args.junit:
        junit(t).write(args.junit)
        args.junit.close()

    if config.only_report_hadrian_deps:
      print("WARNING - skipping all tests and only reporting required hadrian dependencies:", config.hadrian_deps)
      for d in config.hadrian_deps:
        print(d,file=config.only_report_hadrian_deps)
      config.only_report_hadrian_deps.close()

if len(t.unexpected_failures) > 0 or \
   len(t.unexpected_stat_failures) > 0 or \
   len(t.unexpected_passes) > 0 or \
   len(t.framework_failures) > 0:
    exitcode = 1
else:
    exitcode = 0

cleanup_and_exit(exitcode)

# Note [Running tests in /tmp]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use LOCAL=0 to run tests in /tmp, to catch tests that use files from
# the source directory without copying them to the test directory first.
#
# As an example, take a run_command test with a Makefile containing
# `$(TEST_HC) ../Foo.hs`. GHC will now create the output files Foo.o and
# Foo.hi in the source directory. There are 2 problems with this:
# * Output files in the source directory won't get cleaned up automatically.
# * Two tests might (over)write the same output file.
#
# Tests that only fail when run concurrently with other tests are the
# worst, so we try to catch them early by enabling LOCAL=0 in validate.
#
# Adding -outputdir='.' to TEST_HC_OPTS would help a bit, but it requires
# making changes to quite a few tests. The problem is that
# `$(TEST_HC) ../Foo.hs -outputdir=.` with Foo.hs containing
# `module Main where` does not produce Foo.o, as it would without
# -outputdir, but Main.o. See [1].
#
# Using -outputdir='.' is not foolproof anyway, since it does not change
# the destination of the final executable (Foo.exe).
#
# Another hardening method that could be tried is to `chmod -w` the
# source directory.
#
# By default we set LOCAL=1, because it makes it easier to inspect the
# test directory while working on a new test.
#
# [1]
# https://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/separate_compilation.html#output-files
