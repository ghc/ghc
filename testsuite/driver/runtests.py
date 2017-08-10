#!/usr/bin/env python3

#
# (c) Simon Marlow 2002
#

from __future__ import print_function

import argparse
import signal
import sys
import os
import string
import shutil
import tempfile
import time
import re

# We don't actually need subprocess in runtests.py, but:
# * We do need it in testlibs.py
# * We can't import testlibs.py until after we have imported ctypes
# * If we import ctypes before subprocess on cygwin, then sys.exit(0)
#   says "Aborted" and we fail with exit code 134.
# So we import it here first, so that the testsuite doesn't appear to fail.
import subprocess

from testutil import *
from testglobals import *
from junit import junit

# Readline sometimes spews out ANSI escapes for some values of TERM,
# which result in test failures. Thus set TERM to a nice, simple, safe
# value.
os.environ['TERM'] = 'vt100'

global config
config = getConfig() # get it from testglobals

def signal_handler(signal, frame):
        stopNow()

# -----------------------------------------------------------------------------
# cmd-line options

parser = argparse.ArgumentParser(description="GHC's testsuite driver")
perf_group = parser.add_mutually_exclusive_group()

parser.add_argument("-e", action='append', help="A string to execute from the command line.")
parser.add_argument("--config-file", action="append", help="config file")
parser.add_argument("--configfile", action="append", help="config file") # Some old tests have this
parser.add_argument("--config", action='append', help="config field") # Should these config options be mutually exclusive?
parser.add_argument("--rootdir", action='append', help="root of tree containing tests (default: .)") # This should NOT have a default setting or it fucks up duplicates. How to fix?
parser.add_argument("--summary-file", help="file in which to save the (human-readable) summary")
parser.add_argument("--no-print-summary", action="store_true", help="should we print the summary?")
parser.add_argument("--only", action="append", help="just this test (can be give multiple --only= flags)")
parser.add_argument("--way", choices=config.run_ways+config.compile_ways+config.other_ways, help="just this way")
parser.add_argument("--skipway", action="append", choices=config.run_ways+config.compile_ways+config.other_ways, help="skip this way")
parser.add_argument("--threads", type=int, help="threads to run simultaneously")
parser.add_argument("--check-files-written", help="check files aren't written by multiple tests") # NOTE: This doesn't seem to exist?
parser.add_argument("--verbose", type=int, choices=[0,1,2,3,4,5], help="verbose (Values 0 through 5 accepted)")
perf_group.add_argument("--skip-perf-tests", action="store_true", help="skip performance tests")
perf_group.add_argument("--only-perf-tests", action="store_true", help="Only do performance tests")
parser.add_argument("--junit", type=argparse.FileType('wb'), help="output testsuite summary in JUnit format")
parser.add_argument("--use-git-notes", action="store_true", help="use git notes to store metrics. NOTE: This is expected to become the default and will eventually be taken out.")
parser.add_argument("--test-env", default='local', help="Override default chosen test-env.")

args = parser.parse_args()

if args.e:
    for e in args.e:
        exec(e)

if args.configfile:
    for arg in args.configfile:
        exec(open(arg).read())

if args.config_file:
    for arg in args.config_file:
        exec(open(arg).read())

if args.config:
    for arg in args.config:
        field, value = arg.split('=', 1)
        setattr(config, field, value)

if args.rootdir:
    config.rootdirs = args.rootdir


config.summary_file = args.summary_file
config.no_print_summary = args.no_print_summary

if args.only:
    config.only = args.only
    config.run_only_some_tests = True

if args.way:
    config.cmdline_ways = [args.way] + config.cmdline_ways
    if (args.way in config.other_ways):
        config.run_ways = [args.way] + config.run_ways
        config.compile_ways = [args.way] + config.compile_ways

if args.skipway:
    config.other_ways = [w for w in config.other_ways if w != args.skipway]
    config.run_ways = [w for w in config.run_ways if w != args.skipway]
    config.compile_ways = [w for w in config.compile_ways if w != args.skipway]

if args.threads:
    config.threads = args.threads
    config.use_threads = True

if args.verbose:
    config.verbose = args.verbose

# Might need to encase these in if statements.
config.skip_perf_tests = args.skip_perf_tests
config.only_perf_tests = args.only_perf_tests
config.use_git_notes = args.use_git_notes

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
        mydll = ctypes.WinDLL
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
        h = os.popen('locale -a', 'r')
        v = h.read()
        h.close()
        if v != '':
            # If it does then use the first utf8 locale that is available
            h = os.popen('locale -a | grep -i "utf8\|utf-8" 2>/dev/null', 'r')
            v = h.readline().strip()
            h.close()
            if v != '':
                os.environ['LC_ALL'] = v
                print("setting LC_ALL to", v)
            else:
                print('WARNING: No UTF8 locale found.')
                print('You may get some spurious test failures.')

# This has to come after arg parsing as the args can change the compiler
get_compiler_info()

# Can't import this earlier as we need to know if threading will be
# enabled or not
from testlib import *

# On Windows we need to set $PATH to include the paths to all the DLLs
# in order for the dynamic library tests to work.
if windows or darwin:
    pkginfo = str(getStdout([config.ghc_pkg, 'dump']))
    topdir = config.libdir
    if windows:
        mingw = os.path.join(topdir, '../mingw/bin')
        os.environ['PATH'] = os.pathsep.join([os.environ.get("PATH", ""), mingw])
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
            if windows:
                if config.cygwin:
                    # On cygwin we can't put "c:\foo" in $PATH, as : is a
                    # field separator. So convert to /cygdrive/c/foo instead.
                    # Other pythons use ; as the separator, so no problem.
                    path = re.sub('([a-zA-Z]):', '/cygdrive/\\1', path)
                    path = re.sub('\\\\', '/', path)
                os.environ['PATH'] = os.pathsep.join([path, os.environ.get("PATH", "")])
            else:
                # darwin
                os.environ['DYLD_LIBRARY_PATH'] = os.pathsep.join([path, os.environ.get("DYLD_LIBRARY_PATH", "")])

global testopts_local
testopts_local.x = TestOptions()

# if timeout == -1 then we try to calculate a sensible value
if config.timeout == -1:
    config.timeout = int(read_no_crs(config.top + '/timeout/calibrate.out'))

print('Timeout is ' + str(config.timeout))

# -----------------------------------------------------------------------------
# The main dude

if config.rootdirs == []:
    config.rootdirs = ['.']

t_files = list(findTFiles(config.rootdirs))

print('Found', len(t_files), '.T files...')

t = getTestRun()

# Avoid cmd.exe built-in 'date' command on Windows
t.start_time = time.localtime()

print('Beginning test run at', time.strftime("%c %Z",t.start_time))

sys.stdout.flush()
# we output text, which cannot be unbuffered
sys.stdout = os.fdopen(sys.__stdout__.fileno(), "w")

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

# First collect all the tests to be run
t_files_ok = True
for file in t_files:
    if_verbose(2, '====> Scanning %s' % file)
    newTestDir(tempdir, os.path.dirname(file))
    try:
        with io.open(file, encoding='utf8') as f:
            src = f.read()

        exec(src)
    except Exception as e:
        traceback.print_exc()
        framework_fail(file, '', str(e))
        t_files_ok = False

for name in config.only:
    if t_files_ok:
        # See Note [Mutating config.only]
        framework_fail(name, '', 'test not found')
    else:
        # Let user fix .T file errors before reporting on unfound tests.
        # The reson the test can not be found is likely because of those
        # .T file errors.
        pass

if config.list_broken:
    global brokens
    print('')
    print('Broken tests:')
    print(' '.join(map (lambda bdn: '#' + str(bdn[0]) + '(' + bdn[1] + '/' + bdn[2] + ')', brokens)))
    print('')

    if t.framework_failures:
        print('WARNING:', len(framework_failures), 'framework failures!')
        print('')
else:
    # completion watcher
    watcher = Watcher(len(parallelTests))

    # Now run all the tests
    for oneTest in parallelTests:
        if stopping():
            break
        oneTest(watcher)

    # wait for parallel tests to finish
    if not stopping():
        watcher.wait()

    # Run the following tests purely sequential
    config.use_threads = False
    for oneTest in aloneTests:
        if stopping():
            break
        oneTest(watcher)

    # flush everything before we continue
    sys.stdout.flush()

    summary(t, sys.stdout, config.no_print_summary)

    # Write our accumulated metrics into the git notes for this commit.
    if config.use_git_notes:
            note = subprocess.check_output(["git","notes","--ref=perf","append","-m", "\n".join(config.accumulate_metrics)])

    if config.summary_file:
        with open(config.summary_file, 'w') as file:
            summary(t, file)

    if args.junit:
        junit(t).write(args.junit)

cleanup_and_exit(0)

# Note [Running tests in /tmp]
#
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
