# 
# (c) Simon Marlow 2002
#

from __future__ import print_function

import sys
import os
import string
import getopt
import platform
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

PYTHON3 = sys.version_info >= (3, 0)
if PYTHON3:
    print("*** WARNING: running testsuite using Python 3.\n"
          "*** Python 3 support is experimental. See Trac #9184.")

from testutil import *
from testglobals import *

# Readline sometimes spews out ANSI escapes for some values of TERM,
# which result in test failures. Thus set TERM to a nice, simple, safe
# value.
os.environ['TERM'] = 'vt100'

global config
config = getConfig() # get it from testglobals

# -----------------------------------------------------------------------------
# cmd-line options

long_options = [
  "configfile=",	# config file
  "config=",  		# config field
  "rootdir=", 		# root of tree containing tests (default: .)
  "summary-file=",      # file in which to save the (human-readable) summary
  "no-print-summary=",  # should we print the summary?
  "only=",		# just this test (can be give multiple --only= flags)
  "way=",		# just this way
  "skipway=",		# skip this way
  "threads=",           # threads to run simultaneously
  "check-files-written", # check files aren't written by multiple tests
  "verbose=",          # verbose (0,1,2 so far)
  "skip-perf-tests",       # skip performance tests
  ]

opts, args = getopt.getopt(sys.argv[1:], "e:", long_options)
       
for opt,arg in opts:
    if opt == '--configfile':
        exec(open(arg).read())

    # -e is a string to execute from the command line.  For example:
    # testframe -e 'config.compiler=ghc-5.04'
    if opt == '-e':
        exec(arg)

    if opt == '--config':
        field, value = arg.split('=', 1)
        setattr(config, field, value)

    if opt == '--rootdir':
        config.rootdirs.append(arg)

    if opt == '--summary-file':
        config.summary_file = arg

    if opt == '--no-print-summary':
        config.no_print_summary = True

    if opt == '--only':
        config.run_only_some_tests = True
        config.only.add(arg)

    if opt == '--way':
        if (arg not in config.run_ways and arg not in config.compile_ways and arg not in config.other_ways):
            sys.stderr.write("ERROR: requested way \'" +
                             arg + "\' does not exist\n")
            sys.exit(1)
        config.cmdline_ways = [arg] + config.cmdline_ways
        if (arg in config.other_ways):
            config.run_ways = [arg] + config.run_ways
            config.compile_ways = [arg] + config.compile_ways

    if opt == '--skipway':
        if (arg not in config.run_ways and arg not in config.compile_ways and arg not in config.other_ways):
            sys.stderr.write("ERROR: requested way \'" +
                             arg + "\' does not exist\n")
            sys.exit(1)
        config.other_ways = [w for w in config.other_ways if w != arg]
        config.run_ways = [w for w in config.run_ways if w != arg]
        config.compile_ways = [w for w in config.compile_ways if w != arg]

    if opt == '--threads':
        config.threads = int(arg)
        config.use_threads = 1

    if opt == '--skip-perf-tests':
        config.skip_perf_tests = True

    if opt == '--verbose':
        if arg not in ["0","1","2","3","4"]:
            sys.stderr.write("ERROR: requested verbosity %s not supported, use 0,1,2,3 or 4" % arg)
            sys.exit(1)
        config.verbose = int(arg)


if config.use_threads == 1:
    # Trac #1558 says threads don't work in python 2.4.4, but do
    # in 2.5.2. Probably >= 2.5 is sufficient, but let's be
    # conservative here.
    # Some versions of python have things like '1c1' for some of
    # these components (see trac #3091), but int() chokes on the
    # 'c1', so we drop it.
    (maj, min, pat) = platform.python_version_tuple()
    # We wrap maj, min, and pat in str() to work around a bug in python
    # 2.6.1
    maj = int(re.sub('[^0-9].*', '', str(maj)))
    min = int(re.sub('[^0-9].*', '', str(min)))
    pat = int(re.sub('[^0-9].*', '', str(pat)))
    if (maj, min) < (2, 6):
        print("Python < 2.6 is not supported")
        sys.exit(1)
    # We also need to disable threads for python 2.7.2, because of
    # this bug: http://bugs.python.org/issue13817
    elif (maj, min, pat) == (2, 7, 2):
        print("Warning: Ignoring request to use threads as python version is 2.7.2")
        print("See http://bugs.python.org/issue13817 for details.")
        config.use_threads = 0
    if windows: # See Trac ticket #10510.
        print("Warning: Ignoring request to use threads as running on Windows")
        config.use_threads = 0

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
    if hasattr(ctypes, 'windll'):
        mydll = ctypes.windll
    else:
        mydll = ctypes.cdll

    # This actually leaves the terminal in codepage 65001 (UTF8) even
    # after python terminates. We ought really remember the old codepage
    # and set it back.
    if mydll.kernel32.SetConsoleCP(65001) == 0:
        raise Exception("Failure calling SetConsoleCP(65001)")
    if mydll.kernel32.SetConsoleOutputCP(65001) == 0:
        raise Exception("Failure calling SetConsoleOutputCP(65001)")
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
    pkginfo = getStdout([config.ghc_pkg, 'dump'])
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

if config.use_threads:
    t.lock = threading.Lock()
    t.thread_pool = threading.Condition(t.lock)
    t.lockFilesWritten = threading.Lock()
    t.running_threads = 0

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
if PYTHON3:
    # in Python 3, we output text, which cannot be unbuffered
    sys.stdout = os.fdopen(sys.__stdout__.fileno(), "w")
else:
    # set stdout to unbuffered (is this the best way to do it?)
    sys.stdout = os.fdopen(sys.__stdout__.fileno(), "w", 0)

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
        exec(open(file).read())
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
    # Now run all the tests
    if config.use_threads:
        t.running_threads=0
    for oneTest in parallelTests:
        if stopping():
            break
        oneTest()
    if config.use_threads:
        t.thread_pool.acquire()
        while t.running_threads>0:
            t.thread_pool.wait()
        t.thread_pool.release()
    config.use_threads = False
    for oneTest in aloneTests:
        if stopping():
            break
        oneTest()
        
    summary(t, sys.stdout, config.no_print_summary)

    if config.summary_file != '':
        summary(t, open(config.summary_file, 'w'))

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
