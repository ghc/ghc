# coding=utf8
#
# (c) Simon Marlow 2002
#

from __future__ import print_function

import io
import shutil
import os
import errno
import string
import re
import traceback
import time
import datetime
import copy
import glob
import sys
from math import ceil, trunc
import collections
import subprocess

from testglobals import *
from testutil import *
extra_src_files = {'T4198': ['exitminus1.c']} # TODO: See #12223

if config.use_threads:
    import threading
    try:
        import thread
    except ImportError: # Python 3
        import _thread as thread

global wantToStop
wantToStop = False

global pool_sema
if config.use_threads:
    pool_sema = threading.BoundedSemaphore(value=config.threads)

def stopNow():
    global wantToStop
    wantToStop = True
def stopping():
    return wantToStop

# Options valid for the current test only (these get reset to
# testdir_testopts after each test).

global testopts_local
if config.use_threads:
    testopts_local = threading.local()
else:
    class TestOpts_Local:
        pass
    testopts_local = TestOpts_Local()

def getTestOpts():
    return testopts_local.x

def setLocalTestOpts(opts):
    global testopts_local
    testopts_local.x=opts

def isStatsTest():
    opts = getTestOpts()
    return bool(opts.compiler_stats_range_fields or opts.stats_range_fields)


# This can be called at the top of a file of tests, to set default test options
# for the following tests.
def setTestOpts( f ):
    global thisdir_settings
    thisdir_settings = [thisdir_settings, f]

# -----------------------------------------------------------------------------
# Canned setup functions for common cases.  eg. for a test you might say
#
#      test('test001', normal, compile, [''])
#
# to run it without any options, but change it to
#
#      test('test001', expect_fail, compile, [''])
#
# to expect failure for this test.
#
# type TestOpt = (name :: String, opts :: Object) -> IO ()

def normal( name, opts ):
    return;

def skip( name, opts ):
    opts.skip = 1

def expect_fail( name, opts ):
    # The compiler, testdriver, OS or platform is missing a certain
    # feature, and we don't plan to or can't fix it now or in the
    # future.
    opts.expect = 'fail';

def reqlib( lib ):
    return lambda name, opts, l=lib: _reqlib (name, opts, l )

def stage1(name, opts):
    # See Note [Why is there no stage1 setup function?]
    framework_fail(name, 'stage1 setup function does not exist',
                   'add your test to testsuite/tests/stage1 instead')

# Note [Why is there no stage1 setup function?]
#
# Presumably a stage1 setup function would signal that the stage1
# compiler should be used to compile a test.
#
# Trouble is, the path to the compiler + the `ghc --info` settings for
# that compiler are currently passed in from the `make` part of the
# testsuite driver.
#
# Switching compilers in the Python part would be entirely too late, as
# all ghc_with_* settings would be wrong. See config/ghc for possible
# consequences (for example, config.run_ways would still be
# based on the default compiler, quite likely causing ./validate --slow
# to fail).
#
# It would be possible to let the Python part of the testsuite driver
# make the call to `ghc --info`, but doing so would require quite some
# work. Care has to be taken to not affect the run_command tests for
# example, as they also use the `ghc --info` settings:
#     quasiquotation/qq007/Makefile:ifeq "$(GhcDynamic)" "YES"
#
# If you want a test to run using the stage1 compiler, add it to the
# testsuite/tests/stage1 directory. Validate runs the tests in that
# directory with `make stage=1`.

# Cache the results of looking to see if we have a library or not.
# This makes quite a difference, especially on Windows.
have_lib = {}

def _reqlib( name, opts, lib ):
    if lib in have_lib:
        got_it = have_lib[lib]
    else:
        cmd = strip_quotes(config.ghc_pkg)
        p = subprocess.Popen([cmd, '--no-user-package-db', 'describe', lib],
                             stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
        # read from stdout and stderr to avoid blocking due to
        # buffers filling
        p.communicate()
        r = p.wait()
        got_it = r == 0
        have_lib[lib] = got_it

    if not got_it:
        opts.expect = 'missing-lib'

def req_haddock( name, opts ):
    if not config.haddock:
        opts.expect = 'missing-lib'

def req_profiling( name, opts ):
    '''Require the profiling libraries (add 'GhcLibWays += p' to mk/build.mk)'''
    if not config.have_profiling:
        opts.expect = 'fail'

def req_shared_libs( name, opts ):
    if not config.have_shared_libs:
        opts.expect = 'fail'

def req_interp( name, opts ):
    if not config.have_interp:
        opts.expect = 'fail'

def req_smp( name, opts ):
    if not config.have_smp:
        opts.expect = 'fail'

def ignore_stdout(name, opts):
    opts.ignore_stdout = True

def ignore_stderr(name, opts):
    opts.ignore_stderr = True

def combined_output( name, opts ):
    opts.combined_output = True

# -----

def expect_fail_for( ways ):
    return lambda name, opts, w=ways: _expect_fail_for( name, opts, w )

def _expect_fail_for( name, opts, ways ):
    opts.expect_fail_for = ways

def expect_broken( bug ):
    # This test is a expected not to work due to the indicated trac bug
    # number.
    return lambda name, opts, b=bug: _expect_broken (name, opts, b )

def _expect_broken( name, opts, bug ):
    record_broken(name, opts, bug)
    opts.expect = 'fail';

def expect_broken_for( bug, ways ):
    return lambda name, opts, b=bug, w=ways: _expect_broken_for( name, opts, b, w )

def _expect_broken_for( name, opts, bug, ways ):
    record_broken(name, opts, bug)
    opts.expect_fail_for = ways

def record_broken(name, opts, bug):
    global brokens
    me = (bug, opts.testdir, name)
    if not me in brokens:
        brokens.append(me)

def _expect_pass(way):
    # Helper function. Not intended for use in .T files.
    opts = getTestOpts()
    return opts.expect == 'pass' and way not in opts.expect_fail_for

# -----

def omit_ways( ways ):
    return lambda name, opts, w=ways: _omit_ways( name, opts, w )

def _omit_ways( name, opts, ways ):
    opts.omit_ways = ways

# -----

def only_ways( ways ):
    return lambda name, opts, w=ways: _only_ways( name, opts, w )

def _only_ways( name, opts, ways ):
    opts.only_ways = ways

# -----

def extra_ways( ways ):
    return lambda name, opts, w=ways: _extra_ways( name, opts, w )

def _extra_ways( name, opts, ways ):
    opts.extra_ways = ways

# -----

def set_stdin( file ):
   return lambda name, opts, f=file: _set_stdin(name, opts, f);

def _set_stdin( name, opts, f ):
   opts.stdin = f

# -----

def exit_code( val ):
    return lambda name, opts, v=val: _exit_code(name, opts, v);

def _exit_code( name, opts, v ):
    opts.exit_code = v

def signal_exit_code( val ):
    if opsys('solaris2'):
        return exit_code( val );
    else:
        # When application running on Linux receives fatal error
        # signal, then its exit code is encoded as 128 + signal
        # value. See http://www.tldp.org/LDP/abs/html/exitcodes.html
        # I assume that Mac OS X behaves in the same way at least Mac
        # OS X builder behavior suggests this.
        return exit_code( val+128 );

# -----

def compile_timeout_multiplier( val ):
    return lambda name, opts, v=val: _compile_timeout_multiplier(name, opts, v)

def _compile_timeout_multiplier( name, opts, v ):
    opts.compile_timeout_multiplier = v

def run_timeout_multiplier( val ):
    return lambda name, opts, v=val: _run_timeout_multiplier(name, opts, v)

def _run_timeout_multiplier( name, opts, v ):
    opts.run_timeout_multiplier = v

# -----

def extra_run_opts( val ):
    return lambda name, opts, v=val: _extra_run_opts(name, opts, v);

def _extra_run_opts( name, opts, v ):
    opts.extra_run_opts = v

# -----

def extra_hc_opts( val ):
    return lambda name, opts, v=val: _extra_hc_opts(name, opts, v);

def _extra_hc_opts( name, opts, v ):
    opts.extra_hc_opts = v

# -----

def extra_clean( files ):
    # TODO. Remove all calls to extra_clean.
    return lambda _name, _opts: None

def extra_files(files):
    return lambda name, opts: _extra_files(name, opts, files)

def _extra_files(name, opts, files):
    opts.extra_files.extend(files)

# -----

def stats_num_field( field, expecteds ):
    return lambda name, opts, f=field, e=expecteds: _stats_num_field(name, opts, f, e);

def _stats_num_field( name, opts, field, expecteds ):
    if field in opts.stats_range_fields:
        framework_fail(name, 'duplicate-numfield', 'Duplicate ' + field + ' num_field check')

    if type(expecteds) is list:
        for (b, expected, dev) in expecteds:
            if b:
                opts.stats_range_fields[field] = (expected, dev)
                return
        framework_warn(name, 'numfield-no-expected', 'No expected value found for ' + field + ' in num_field check')

    else:
        (expected, dev) = expecteds
        opts.stats_range_fields[field] = (expected, dev)

def compiler_stats_num_field( field, expecteds ):
    return lambda name, opts, f=field, e=expecteds: _compiler_stats_num_field(name, opts, f, e);

def _compiler_stats_num_field( name, opts, field, expecteds ):
    if field in opts.compiler_stats_range_fields:
        framework_fail(name, 'duplicate-numfield', 'Duplicate ' + field + ' num_field check')

    # Compiler performance numbers change when debugging is on, making the results
    # useless and confusing. Therefore, skip if debugging is on.
    if compiler_debugged():
        skip(name, opts)

    for (b, expected, dev) in expecteds:
        if b:
            opts.compiler_stats_range_fields[field] = (expected, dev)
            return

    framework_warn(name, 'numfield-no-expected', 'No expected value found for ' + field + ' in num_field check')

# -----

def when(b, f):
    # When list_brokens is on, we want to see all expect_broken calls,
    # so we always do f
    if b or config.list_broken:
        return f
    else:
        return normal

def unless(b, f):
    return when(not b, f)

def doing_ghci():
    return 'ghci' in config.run_ways

def ghc_dynamic():
    return config.ghc_dynamic

def fast():
    return config.speed == 2

def platform( plat ):
    return config.platform == plat

def opsys( os ):
    return config.os == os

def arch( arch ):
    return config.arch == arch

def wordsize( ws ):
    return config.wordsize == str(ws)

def msys( ):
    return config.msys

def cygwin( ):
    return config.cygwin

def have_vanilla( ):
    return config.have_vanilla

def have_dynamic( ):
    return config.have_dynamic

def have_profiling( ):
    return config.have_profiling

def in_tree_compiler( ):
    return config.in_tree_compiler

def unregisterised( ):
    return config.unregisterised

def compiler_profiled( ):
    return config.compiler_profiled

def compiler_debugged( ):
    return config.compiler_debugged

# ---

def high_memory_usage(name, opts):
    opts.alone = True

# If a test is for a multi-CPU race, then running the test alone
# increases the chance that we'll actually see it.
def multi_cpu_race(name, opts):
    opts.alone = True

# ---
def literate( name, opts ):
    opts.literate = 1;

def c_src( name, opts ):
    opts.c_src = 1;

def objc_src( name, opts ):
    opts.objc_src = 1;

def objcpp_src( name, opts ):
    opts.objcpp_src = 1;

def cmm_src( name, opts ):
    opts.cmm_src = 1;

def outputdir( odir ):
    return lambda name, opts, d=odir: _outputdir(name, opts, d)

def _outputdir( name, opts, odir ):
    opts.outputdir = odir;

# ----

def pre_cmd( cmd ):
    return lambda name, opts, c=cmd: _pre_cmd(name, opts, cmd)

def _pre_cmd( name, opts, cmd ):
    opts.pre_cmd = cmd

# ----

def clean_cmd( cmd ):
    # TODO. Remove all calls to clean_cmd.
    return lambda _name, _opts: None

# ----

def cmd_prefix( prefix ):
    return lambda name, opts, p=prefix: _cmd_prefix(name, opts, prefix)

def _cmd_prefix( name, opts, prefix ):
    opts.cmd_wrapper = lambda cmd, p=prefix: p + ' ' + cmd;

# ----

def cmd_wrapper( fun ):
    return lambda name, opts, f=fun: _cmd_wrapper(name, opts, fun)

def _cmd_wrapper( name, opts, fun ):
    opts.cmd_wrapper = fun

# ----

def compile_cmd_prefix( prefix ):
    return lambda name, opts, p=prefix: _compile_cmd_prefix(name, opts, prefix)

def _compile_cmd_prefix( name, opts, prefix ):
    opts.compile_cmd_prefix = prefix

# ----

def check_stdout( f ):
    return lambda name, opts, f=f: _check_stdout(name, opts, f)

def _check_stdout( name, opts, f ):
    opts.check_stdout = f

def no_check_hp(name, opts):
    opts.check_hp = False

# ----

def filter_stdout_lines( regex ):
    """ Filter lines of stdout with the given regular expression """
    import re
    def f( name, opts ):
        _normalise_fun(name, opts, lambda s: '\n'.join(re.findall(regex, s)))
    return f

def normalise_slashes( name, opts ):
    _normalise_fun(name, opts, normalise_slashes_)

def normalise_exe( name, opts ):
    _normalise_fun(name, opts, normalise_exe_)

def normalise_fun( *fs ):
    return lambda name, opts: _normalise_fun(name, opts, fs)

def _normalise_fun( name, opts, *fs ):
    opts.extra_normaliser = join_normalisers(opts.extra_normaliser, fs)

def normalise_errmsg_fun( *fs ):
    return lambda name, opts: _normalise_errmsg_fun(name, opts, fs)

def _normalise_errmsg_fun( name, opts, *fs ):
    opts.extra_errmsg_normaliser =  join_normalisers(opts.extra_errmsg_normaliser, fs)

def normalise_whitespace_fun(f):
    return lambda name, opts: _normalise_whitespace_fun(name, opts, f)

def _normalise_whitespace_fun(name, opts, f):
    opts.whitespace_normaliser = f

def normalise_version_( *pkgs ):
    def normalise_version__( str ):
        return re.sub('(' + '|'.join(map(re.escape,pkgs)) + ')-[0-9.]+',
                      '\\1-<VERSION>', str)
    return normalise_version__

def normalise_version( *pkgs ):
    def normalise_version__( name, opts ):
        _normalise_fun(name, opts, normalise_version_(*pkgs))
        _normalise_errmsg_fun(name, opts, normalise_version_(*pkgs))
    return normalise_version__

def normalise_drive_letter(name, opts):
    # Windows only. Change D:\\ to C:\\.
    _normalise_fun(name, opts, lambda str: re.sub(r'[A-Z]:\\', r'C:\\', str))

def keep_prof_callstacks(name, opts):
    """Keep profiling callstacks.

    Use together with `only_ways(prof_ways)`.
    """
    opts.keep_prof_callstacks = True

def join_normalisers(*a):
    """
    Compose functions, flattening sequences.

       join_normalisers(f1,[f2,f3],f4)

    is the same as

       lambda x: f1(f2(f3(f4(x))))
    """

    def flatten(l):
        """
        Taken from http://stackoverflow.com/a/2158532/946226
        """
        for el in l:
            if (isinstance(el, collections.Iterable)
                and not isinstance(el, (bytes, str))):
                for sub in flatten(el):
                    yield sub
            else:
                yield el

    a = flatten(a)

    fn = lambda x:x # identity function
    for f in a:
        assert callable(f)
        fn = lambda x,f=f,fn=fn: fn(f(x))
    return fn

# ----
# Function for composing two opt-fns together

def executeSetups(fs, name, opts):
    if type(fs) is list:
        # If we have a list of setups, then execute each one
        for f in fs:
            executeSetups(f, name, opts)
    else:
        # fs is a single function, so just apply it
        fs(name, opts)

# -----------------------------------------------------------------------------
# The current directory of tests

def newTestDir(tempdir, dir):

    global thisdir_settings
    # reset the options for this test directory
    def settings(name, opts, tempdir=tempdir, dir=dir):
        return _newTestDir(name, opts, tempdir, dir)
    thisdir_settings = settings

# Should be equal to entry in toplevel .gitignore.
testdir_suffix = '.run'

def _newTestDir(name, opts, tempdir, dir):
    opts.srcdir = os.path.join(os.getcwd(), dir)
    opts.testdir = os.path.join(tempdir, dir, name + testdir_suffix)
    opts.compiler_always_flags = config.compiler_always_flags

# -----------------------------------------------------------------------------
# Actually doing tests

parallelTests = []
aloneTests = []
allTestNames = set([])

def runTest(watcher, opts, name, func, args):
    if config.use_threads:
        pool_sema.acquire()
        t = threading.Thread(target=test_common_thread,
                             name=name,
                             args=(watcher, name, opts, func, args))
        t.daemon = False
        t.start()
    else:
        test_common_work(watcher, name, opts, func, args)

# name  :: String
# setup :: [TestOpt] -> IO ()
def test(name, setup, func, args):
    global aloneTests
    global parallelTests
    global allTestNames
    global thisdir_settings
    if name in allTestNames:
        framework_fail(name, 'duplicate', 'There are multiple tests with this name')
    if not re.match('^[0-9]*[a-zA-Z][a-zA-Z0-9._-]*$', name):
        framework_fail(name, 'bad_name', 'This test has an invalid name')

    if config.run_only_some_tests:
        if name not in config.only:
            return
        else:
            # Note [Mutating config.only]
            # config.only is initially the set of tests requested by
            # the user (via 'make TEST='). We then remove all tests that
            # we've already seen (in .T files), so that we can later
            # report on any tests we couldn't find and error out.
            config.only.remove(name)

    # Make a deep copy of the default_testopts, as we need our own copy
    # of any dictionaries etc inside it. Otherwise, if one test modifies
    # them, all tests will see the modified version!
    myTestOpts = copy.deepcopy(default_testopts)

    executeSetups([thisdir_settings, setup], name, myTestOpts)

    thisTest = lambda watcher: runTest(watcher, myTestOpts, name, func, args)
    if myTestOpts.alone:
        aloneTests.append(thisTest)
    else:
        parallelTests.append(thisTest)
    allTestNames.add(name)

if config.use_threads:
    def test_common_thread(watcher, name, opts, func, args):
            try:
                test_common_work(watcher, name, opts, func, args)
            finally:
                pool_sema.release()

def get_package_cache_timestamp():
    if config.package_conf_cache_file == '':
        return 0.0
    else:
        try:
            return os.stat(config.package_conf_cache_file).st_mtime
        except:
            return 0.0

do_not_copy = ('.hi', '.o', '.dyn_hi', '.dyn_o', '.out') # 12112

def test_common_work(watcher, name, opts, func, args):
    try:
        t.total_tests += 1
        setLocalTestOpts(opts)

        package_conf_cache_file_start_timestamp = get_package_cache_timestamp()

        # All the ways we might run this test
        if func == compile or func == multimod_compile:
            all_ways = config.compile_ways
        elif func == compile_and_run or func == multimod_compile_and_run:
            all_ways = config.run_ways
        elif func == ghci_script:
            if 'ghci' in config.run_ways:
                all_ways = ['ghci']
            else:
                all_ways = []
        else:
            all_ways = ['normal']

        # A test itself can request extra ways by setting opts.extra_ways
        all_ways = all_ways + [way for way in opts.extra_ways if way not in all_ways]

        t.total_test_cases += len(all_ways)

        ok_way = lambda way: \
            not getTestOpts().skip \
            and (getTestOpts().only_ways == None or way in getTestOpts().only_ways) \
            and (config.cmdline_ways == [] or way in config.cmdline_ways) \
            and (not (config.skip_perf_tests and isStatsTest())) \
            and way not in getTestOpts().omit_ways

        # Which ways we are asked to skip
        do_ways = list(filter (ok_way,all_ways))

        # Only run all ways in slow mode.
        # See Note [validate and testsuite speed] in toplevel Makefile.
        if config.accept:
            # Only ever run one way
            do_ways = do_ways[:1]
        elif config.speed > 0:
            # However, if we EXPLICITLY asked for a way (with extra_ways)
            # please test it!
            explicit_ways = list(filter(lambda way: way in opts.extra_ways, do_ways))
            other_ways = list(filter(lambda way: way not in opts.extra_ways, do_ways))
            do_ways = other_ways[:1] + explicit_ways

        # Find all files in the source directory that this test
        # depends on. Do this only once for all ways.
        # Generously add all filenames that start with the name of
        # the test to this set, as a convenience to test authors.
        # They will have to use the `extra_files` setup function to
        # specify all other files that their test depends on (but
        # this seems to be necessary for only about 10% of all
        # tests).
        files = set(f for f in os.listdir(opts.srcdir)
                       if f.startswith(name) and not f == name and
                          not f.endswith(testdir_suffix) and
                          not os.path.splitext(f)[1] in do_not_copy)
        for filename in (opts.extra_files + extra_src_files.get(name, [])):
            if filename.startswith('/'):
                framework_fail(name, 'whole-test',
                    'no absolute paths in extra_files please: ' + filename)

            elif '*' in filename:
                # Don't use wildcards in extra_files too much, as
                # globbing is slow.
                files.update((os.path.relpath(f, opts.srcdir)
                            for f in glob.iglob(in_srcdir(filename))))

            elif filename:
                files.add(filename)

            else:
                framework_fail(name, 'whole-test', 'extra_file is empty string')

        # Run the required tests...
        for way in do_ways:
            if stopping():
                break
            try:
                do_test(name, way, func, args, files)
            except KeyboardInterrupt:
                stopNow()
            except Exception as e:
                framework_fail(name, way, str(e))
                traceback.print_exc()

        t.n_tests_skipped += len(set(all_ways) - set(do_ways))

        if config.cleanup and do_ways:
            try:
                cleanup()
            except Exception as e:
                framework_fail(name, 'runTest', 'Unhandled exception during cleanup: ' + str(e))

        package_conf_cache_file_end_timestamp = get_package_cache_timestamp();

        if package_conf_cache_file_start_timestamp != package_conf_cache_file_end_timestamp:
            framework_fail(name, 'whole-test', 'Package cache timestamps do not match: ' + str(package_conf_cache_file_start_timestamp) + ' ' + str(package_conf_cache_file_end_timestamp))

    except Exception as e:
        framework_fail(name, 'runTest', 'Unhandled exception: ' + str(e))
    finally:
        watcher.notify()

def do_test(name, way, func, args, files):
    opts = getTestOpts()

    full_name = name + '(' + way + ')'

    if_verbose(2, "=====> {0} {1} of {2} {3}".format(
        full_name, t.total_tests, len(allTestNames), 
        [len(t.unexpected_passes),
         len(t.unexpected_failures),
         len(t.framework_failures)]))

    # Clean up prior to the test, so that we can't spuriously conclude
    # that it passed on the basis of old run outputs.
    cleanup()
    os.makedirs(opts.testdir)

    # Link all source files for this test into a new directory in
    # /tmp, and run the test in that directory. This makes it
    # possible to run tests in parallel, without modification, that
    # would otherwise (accidentally) write to the same output file.
    # It also makes it easier to keep the testsuite clean.

    for extra_file in files:
        src = in_srcdir(extra_file)
        dst = in_testdir(os.path.basename(extra_file.rstrip('/\\')))
        if os.path.isfile(src):
            link_or_copy_file(src, dst)
        elif os.path.isdir(src):
            os.mkdir(dst)
            lndir(src, dst)
        else:
            if not config.haddock and os.path.splitext(extra_file)[1] == '.t':
                # When using a ghc built without haddock support, .t
                # files are rightfully missing. Don't
                # framework_fail. Test will be skipped later.
                pass
            else:
                framework_fail(name, way,
                    'extra_file does not exist: ' + extra_file)

    if func.__name__ == 'run_command' or opts.pre_cmd:
        # When running 'MAKE' make sure 'TOP' still points to the
        # root of the testsuite.
        src_makefile = in_srcdir('Makefile')
        dst_makefile = in_testdir('Makefile')
        if os.path.exists(src_makefile):
            with io.open(src_makefile, 'r', encoding='utf8') as src:
                makefile = re.sub('TOP=.*', 'TOP=' + config.top, src.read(), 1)
                with io.open(dst_makefile, 'w', encoding='utf8') as dst:
                    dst.write(makefile)

    if opts.pre_cmd:
        exit_code = runCmd('cd "{0}" && {1}'.format(opts.testdir, override_options(opts.pre_cmd)),
                           stderr = subprocess.STDOUT,
                           print_output = config.verbose >= 3)

        if exit_code != 0:
            framework_fail(name, way, 'pre_cmd failed: {0}'.format(exit_code))

    result = func(*[name,way] + args)

    if opts.expect not in ['pass', 'fail', 'missing-lib']:
        framework_fail(name, way, 'bad expected ' + opts.expect)

    try:
        passFail = result['passFail']
    except (KeyError, TypeError):
        passFail = 'No passFail found'

    directory = re.sub('^\\.[/\\\\]', '', opts.testdir)

    if passFail == 'pass':
        if _expect_pass(way):
            t.expected_passes.append((directory, name, way))
            t.n_expected_passes += 1
        else:
            if_verbose(1, '*** unexpected pass for %s' % full_name)
            t.unexpected_passes.append((directory, name, 'unexpected', way))
    elif passFail == 'fail':
        if _expect_pass(way):
            reason = result['reason']
            tag = result.get('tag')
            if tag == 'stat':
                if_verbose(1, '*** unexpected stat test failure for %s' % full_name)
                t.unexpected_stat_failures.append((directory, name, reason, way))
            else:
                if_verbose(1, '*** unexpected failure for %s' % full_name)
                t.unexpected_failures.append((directory, name, reason, way))
        else:
            if opts.expect == 'missing-lib':
                t.missing_libs.append((directory, name, 'missing-lib', way))
            else:
                t.n_expected_failures += 1
    else:
        framework_fail(name, way, 'bad result ' + passFail)

# Make is often invoked with -s, which means if it fails, we get
# no feedback at all. This is annoying. So let's remove the option
# if found and instead have the testsuite decide on what to do
# with the output.
def override_options(pre_cmd):
    if config.verbose >= 5 and bool(re.match('\$make', pre_cmd, re.I)):
        return pre_cmd.replace('-s'      , '') \
                      .replace('--silent', '') \
                      .replace('--quiet' , '')

    return pre_cmd

def framework_fail(name, way, reason):
    opts = getTestOpts()
    directory = re.sub('^\\.[/\\\\]', '', opts.testdir)
    full_name = name + '(' + way + ')'
    if_verbose(1, '*** framework failure for %s %s ' % (full_name, reason))
    t.framework_failures.append((directory, name, way, reason))

def framework_warn(name, way, reason):
    opts = getTestOpts()
    directory = re.sub('^\\.[/\\\\]', '', opts.testdir)
    full_name = name + '(' + way + ')'
    if_verbose(1, '*** framework warning for %s %s ' % (full_name, reason))
    t.framework_warnings.append((directory, name, way, reason))

def badResult(result):
    try:
        if result['passFail'] == 'pass':
            return False
        return True
    except (KeyError, TypeError):
        return True

def passed():
    return {'passFail': 'pass'}

def failBecause(reason, tag=None):
    return {'passFail': 'fail', 'reason': reason, 'tag': tag}

# -----------------------------------------------------------------------------
# Generic command tests

# A generic command test is expected to run and exit successfully.
#
# The expected exit code can be changed via exit_code() as normal, and
# the expected stdout/stderr are stored in <testname>.stdout and
# <testname>.stderr.  The output of the command can be ignored
# altogether by using the setup function ignore_stdout instead of
# run_command.

def run_command( name, way, cmd ):
    return simple_run( name, '', override_options(cmd), '' )

# -----------------------------------------------------------------------------
# GHCi tests

def ghci_script( name, way, script):
    flags = ' '.join(get_compiler_flags())
    way_flags = ' '.join(config.way_flags[way])

    # We pass HC and HC_OPTS as environment variables, so that the
    # script can invoke the correct compiler by using ':! $HC $HC_OPTS'
    cmd = ('HC={{compiler}} HC_OPTS="{flags}" {{compiler}} {flags} {way_flags}'
          ).format(flags=flags, way_flags=way_flags)

    getTestOpts().stdin = script
    return simple_run( name, way, cmd, getTestOpts().extra_run_opts )

# -----------------------------------------------------------------------------
# Compile-only tests

def compile( name, way, extra_hc_opts ):
    return do_compile( name, way, 0, '', [], extra_hc_opts )

def compile_fail( name, way, extra_hc_opts ):
    return do_compile( name, way, 1, '', [], extra_hc_opts )

def backpack_typecheck( name, way, extra_hc_opts ):
    return do_compile( name, way, 0, '', [], "-fno-code -fwrite-interface " + extra_hc_opts, backpack=1 )

def backpack_typecheck_fail( name, way, extra_hc_opts ):
    return do_compile( name, way, 1, '', [], "-fno-code -fwrite-interface " + extra_hc_opts, backpack=1 )

def backpack_compile( name, way, extra_hc_opts ):
    return do_compile( name, way, 0, '', [], extra_hc_opts, backpack=1 )

def backpack_compile_fail( name, way, extra_hc_opts ):
    return do_compile( name, way, 1, '', [], extra_hc_opts, backpack=1 )

def backpack_run( name, way, extra_hc_opts ):
    return compile_and_run__( name, way, '', [], extra_hc_opts, backpack=1 )

def multimod_compile( name, way, top_mod, extra_hc_opts ):
    return do_compile( name, way, 0, top_mod, [], extra_hc_opts )

def multimod_compile_fail( name, way, top_mod, extra_hc_opts ):
    return do_compile( name, way, 1, top_mod, [], extra_hc_opts )

def multi_compile( name, way, top_mod, extra_mods, extra_hc_opts ):
    return do_compile( name, way, 0, top_mod, extra_mods, extra_hc_opts)

def multi_compile_fail( name, way, top_mod, extra_mods, extra_hc_opts ):
    return do_compile( name, way, 1, top_mod, extra_mods, extra_hc_opts)

def do_compile(name, way, should_fail, top_mod, extra_mods, extra_hc_opts, **kwargs):
    # print 'Compile only, extra args = ', extra_hc_opts

    result = extras_build( way, extra_mods, extra_hc_opts )
    if badResult(result):
       return result
    extra_hc_opts = result['hc_opts']

    result = simple_build(name, way, extra_hc_opts, should_fail, top_mod, 0, 1, **kwargs)

    if badResult(result):
        return result

    # the actual stderr should always match the expected, regardless
    # of whether we expected the compilation to fail or not (successful
    # compilations may generate warnings).

    expected_stderr_file = find_expected_file(name, 'stderr')
    actual_stderr_file = add_suffix(name, 'comp.stderr')

    if not compare_outputs(way, 'stderr',
                           join_normalisers(getTestOpts().extra_errmsg_normaliser,
                                            normalise_errmsg),
                           expected_stderr_file, actual_stderr_file,
                           whitespace_normaliser=getattr(getTestOpts(),
                                                         "whitespace_normaliser",
                                                         normalise_whitespace)):
        return failBecause('stderr mismatch')

    # no problems found, this test passed
    return passed()

def compile_cmp_asm( name, way, extra_hc_opts ):
    print('Compile only, extra args = ', extra_hc_opts)
    result = simple_build(name + '.cmm', way, '-keep-s-files -O ' + extra_hc_opts, 0, '', 0, 0)

    if badResult(result):
        return result

    # the actual stderr should always match the expected, regardless
    # of whether we expected the compilation to fail or not (successful
    # compilations may generate warnings).

    expected_asm_file = find_expected_file(name, 'asm')
    actual_asm_file = add_suffix(name, 's')

    if not compare_outputs(way, 'asm',
                           join_normalisers(normalise_errmsg, normalise_asm),
                           expected_asm_file, actual_asm_file):
        return failBecause('asm mismatch')

    # no problems found, this test passed
    return passed()

# -----------------------------------------------------------------------------
# Compile-and-run tests

def compile_and_run__( name, way, top_mod, extra_mods, extra_hc_opts, backpack=0 ):
    # print 'Compile and run, extra args = ', extra_hc_opts

    result = extras_build( way, extra_mods, extra_hc_opts )
    if badResult(result):
       return result
    extra_hc_opts = result['hc_opts']

    if way.startswith('ghci'): # interpreted...
        return interpreter_run(name, way, extra_hc_opts, top_mod)
    else: # compiled...
        result = simple_build(name, way, extra_hc_opts, 0, top_mod, 1, 1, backpack = backpack)
        if badResult(result):
            return result

        cmd = './' + name;

        # we don't check the compiler's stderr for a compile-and-run test
        return simple_run( name, way, cmd, getTestOpts().extra_run_opts )

def compile_and_run( name, way, extra_hc_opts ):
    return compile_and_run__( name, way, '', [], extra_hc_opts)

def multimod_compile_and_run( name, way, top_mod, extra_hc_opts ):
    return compile_and_run__( name, way, top_mod, [], extra_hc_opts)

def multi_compile_and_run( name, way, top_mod, extra_mods, extra_hc_opts ):
    return compile_and_run__( name, way, top_mod, extra_mods, extra_hc_opts)

def stats( name, way, stats_file ):
    opts = getTestOpts()
    return checkStats(name, way, stats_file, opts.stats_range_fields)

# -----------------------------------------------------------------------------
# Check -t stats info

def checkStats(name, way, stats_file, range_fields):
    full_name = name + '(' + way + ')'

    result = passed()
    if range_fields:
        try:
            f = open(in_testdir(stats_file))
        except IOError as e:
            return failBecause(str(e))
        contents = f.read()
        f.close()

        for (field, (expected, dev)) in range_fields.items():
            m = re.search('\("' + field + '", "([0-9]+)"\)', contents)
            if m == None:
                print('Failed to find field: ', field)
                result = failBecause('no such stats field')
            val = int(m.group(1))

            lowerBound = trunc(           expected * ((100 - float(dev))/100))
            upperBound = trunc(0.5 + ceil(expected * ((100 + float(dev))/100)))

            deviation = round(((float(val) * 100)/ expected) - 100, 1)

            if val < lowerBound:
                print(field, 'value is too low:')
                print('(If this is because you have improved GHC, please')
                print('update the test so that GHC doesn\'t regress again)')
                result = failBecause('stat too good', tag='stat')
            if val > upperBound:
                print(field, 'value is too high:')
                result = failBecause('stat not good enough', tag='stat')

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

# -----------------------------------------------------------------------------
# Build a single-module program

def extras_build( way, extra_mods, extra_hc_opts ):
    for mod, opts in extra_mods:
        result = simple_build(mod, way, opts + ' ' + extra_hc_opts, 0, '', 0, 0)
        if not (mod.endswith('.hs') or mod.endswith('.lhs')):
            extra_hc_opts += ' ' + replace_suffix(mod, 'o')
        if badResult(result):
            return result

    return {'passFail' : 'pass', 'hc_opts' : extra_hc_opts}

def simple_build(name, way, extra_hc_opts, should_fail, top_mod, link, addsuf, backpack = False):
    opts = getTestOpts()

    # Redirect stdout and stderr to the same file
    stdout = in_testdir(name, 'comp.stderr')
    stderr = subprocess.STDOUT

    if top_mod != '':
        srcname = top_mod
    elif addsuf:
        if backpack:
            srcname = add_suffix(name, 'bkp')
        else:
            srcname = add_hs_lhs_suffix(name)
    else:
        srcname = name

    if top_mod != '':
        to_do = '--make '
        if link:
            to_do = to_do + '-o ' + name
    elif backpack:
        if link:
            to_do = '-o ' + name + ' '
        else:
            to_do = ''
        to_do = to_do + '--backpack '
    elif link:
        to_do = '-o ' + name
    else:
        to_do = '-c' # just compile

    stats_file = name + '.comp.stats'
    if opts.compiler_stats_range_fields:
        extra_hc_opts += ' +RTS -V0 -t' + stats_file + ' --machine-readable -RTS'
    if backpack:
        extra_hc_opts += ' -outputdir ' + name + '.out'

    # Required by GHC 7.3+, harmless for earlier versions:
    if (getTestOpts().c_src or
        getTestOpts().objc_src or
        getTestOpts().objcpp_src or
        getTestOpts().cmm_src):
        extra_hc_opts += ' -no-hs-main '

    if getTestOpts().compile_cmd_prefix == '':
        cmd_prefix = ''
    else:
        cmd_prefix = getTestOpts().compile_cmd_prefix + ' '

    flags = ' '.join(get_compiler_flags() + config.way_flags[way])

    cmd = ('cd "{opts.testdir}" && {cmd_prefix} '
           '{{compiler}} {to_do} {srcname} {flags} {extra_hc_opts}'
          ).format(**locals())

    exit_code = runCmd(cmd, None, stdout, stderr, opts.compile_timeout_multiplier)

    if exit_code != 0 and not should_fail:
        if config.verbose >= 1 and _expect_pass(way):
            print('Compile failed (exit code {0}) errors were:'.format(exit_code))
            actual_stderr_path = in_testdir(name, 'comp.stderr')
            if_verbose_dump(1, actual_stderr_path)

    # ToDo: if the sub-shell was killed by ^C, then exit

    statsResult = checkStats(name, way, stats_file, opts.compiler_stats_range_fields)

    if badResult(statsResult):
        return statsResult

    if should_fail:
        if exit_code == 0:
            return failBecause('exit code 0')
    else:
        if exit_code != 0:
            return failBecause('exit code non-0')

    return passed()

# -----------------------------------------------------------------------------
# Run a program and check its output
#
# If testname.stdin exists, route input from that, else
# from /dev/null.  Route output to testname.run.stdout and
# testname.run.stderr.  Returns the exit code of the run.

def simple_run(name, way, prog, extra_run_opts):
    opts = getTestOpts()

    # figure out what to use for stdin
    if opts.stdin:
        stdin = in_testdir(opts.stdin)
    elif os.path.exists(in_testdir(name, 'stdin')):
        stdin = in_testdir(name, 'stdin')
    else:
        stdin = None

    stdout = in_testdir(name, 'run.stdout')
    if opts.combined_output:
        stderr = subprocess.STDOUT
    else:
        stderr = in_testdir(name, 'run.stderr')

    my_rts_flags = rts_flags(way)

    stats_file = name + '.stats'
    if opts.stats_range_fields:
        stats_args = ' +RTS -V0 -t' + stats_file + ' --machine-readable -RTS'
    else:
        stats_args = ''

    # Put extra_run_opts last: extra_run_opts('+RTS foo') should work.
    cmd = prog + stats_args + ' ' + my_rts_flags + ' ' + extra_run_opts

    if opts.cmd_wrapper != None:
        cmd = opts.cmd_wrapper(cmd)

    cmd = 'cd "{opts.testdir}" && {cmd}'.format(**locals())

    # run the command
    exit_code = runCmd(cmd, stdin, stdout, stderr, opts.run_timeout_multiplier)

    # check the exit code
    if exit_code != opts.exit_code:
        if config.verbose >= 1 and _expect_pass(way):
            print('Wrong exit code for ' + name + '(' + way + ')' + '(expected', opts.exit_code, ', actual', exit_code, ')')
            dump_stdout(name)
            dump_stderr(name)
        return failBecause('bad exit code')

    if not (opts.ignore_stderr or stderr_ok(name, way) or opts.combined_output):
        return failBecause('bad stderr')
    if not (opts.ignore_stdout or stdout_ok(name, way)):
        return failBecause('bad stdout')

    check_hp = '-h' in my_rts_flags and opts.check_hp
    check_prof = '-p' in my_rts_flags

    # exit_code > 127 probably indicates a crash, so don't try to run hp2ps.
    if check_hp and (exit_code <= 127 or exit_code == 251) and not check_hp_ok(name):
        return failBecause('bad heap profile')
    if check_prof and not check_prof_ok(name, way):
        return failBecause('bad profile')

    return checkStats(name, way, stats_file, opts.stats_range_fields)

def rts_flags(way):
    args = config.way_rts_flags.get(way, [])
    return '+RTS {0} -RTS'.format(' '.join(args)) if args else ''

# -----------------------------------------------------------------------------
# Run a program in the interpreter and check its output

def interpreter_run(name, way, extra_hc_opts, top_mod):
    opts = getTestOpts()

    stdout = in_testdir(name, 'interp.stdout')
    stderr = in_testdir(name, 'interp.stderr')
    script = in_testdir(name, 'genscript')

    if opts.combined_output:
        framework_fail(name, 'unsupported',
                       'WAY=ghci and combined_output together is not supported')

    if (top_mod == ''):
        srcname = add_hs_lhs_suffix(name)
    else:
        srcname = top_mod

    delimiter = '===== program output begins here\n'

    with io.open(script, 'w', encoding='utf8') as f:
        # set the prog name and command-line args to match the compiled
        # environment.
        f.write(':set prog ' + name + '\n')
        f.write(':set args ' + opts.extra_run_opts + '\n')
        # Add marker lines to the stdout and stderr output files, so we
        # can separate GHCi's output from the program's.
        f.write(':! echo ' + delimiter)
        f.write(':! echo 1>&2 ' + delimiter)
        # Set stdout to be line-buffered to match the compiled environment.
        f.write('System.IO.hSetBuffering System.IO.stdout System.IO.LineBuffering\n')
        # wrapping in GHC.TopHandler.runIO ensures we get the same output
        # in the event of an exception as for the compiled program.
        f.write('GHC.TopHandler.runIOFastExit Main.main Prelude.>> Prelude.return ()\n')

    stdin = in_testdir(opts.stdin if opts.stdin else add_suffix(name, 'stdin'))
    if os.path.exists(stdin):
        os.system('cat "{0}" >> "{1}"'.format(stdin, script))

    flags = ' '.join(get_compiler_flags() + config.way_flags[way])

    cmd = ('{{compiler}} {srcname} {flags} {extra_hc_opts}'
          ).format(**locals())

    if getTestOpts().cmd_wrapper != None:
        cmd = opts.cmd_wrapper(cmd);

    cmd = 'cd "{opts.testdir}" && {cmd}'.format(**locals())

    exit_code = runCmd(cmd, script, stdout, stderr, opts.run_timeout_multiplier)

    # split the stdout into compilation/program output
    split_file(stdout, delimiter,
               in_testdir(name, 'comp.stdout'),
               in_testdir(name, 'run.stdout'))
    split_file(stderr, delimiter,
               in_testdir(name, 'comp.stderr'),
               in_testdir(name, 'run.stderr'))

    # check the exit code
    if exit_code != getTestOpts().exit_code:
        print('Wrong exit code for ' + name + '(' + way + ') (expected', getTestOpts().exit_code, ', actual', exit_code, ')')
        dump_stdout(name)
        dump_stderr(name)
        return failBecause('bad exit code')

    # ToDo: if the sub-shell was killed by ^C, then exit

    if not (opts.ignore_stderr or stderr_ok(name, way)):
        return failBecause('bad stderr')
    elif not (opts.ignore_stdout or stdout_ok(name, way)):
        return failBecause('bad stdout')
    else:
        return passed()

def split_file(in_fn, delimiter, out1_fn, out2_fn):
    # See Note [Universal newlines].
    with io.open(in_fn, 'r', encoding='utf8', errors='replace', newline=None) as infile:
        with io.open(out1_fn, 'w', encoding='utf8', newline='') as out1:
            with io.open(out2_fn, 'w', encoding='utf8', newline='') as out2:
                line = infile.readline()
                while re.sub('^\s*','',line) != delimiter and line != '':
                    out1.write(line)
                    line = infile.readline()

                line = infile.readline()
                while line != '':
                    out2.write(line)
                    line = infile.readline()

# -----------------------------------------------------------------------------
# Utils
def get_compiler_flags():
    opts = getTestOpts()

    flags = copy.copy(opts.compiler_always_flags)

    flags.append(opts.extra_hc_opts)

    if opts.outputdir != None:
        flags.extend(["-outputdir", opts.outputdir])

    return flags

def stdout_ok(name, way):
   actual_stdout_file = add_suffix(name, 'run.stdout')
   expected_stdout_file = find_expected_file(name, 'stdout')

   extra_norm = join_normalisers(normalise_output, getTestOpts().extra_normaliser)

   check_stdout = getTestOpts().check_stdout
   if check_stdout:
      actual_stdout_path = in_testdir(actual_stdout_file)
      return check_stdout(actual_stdout_path, extra_norm)

   return compare_outputs(way, 'stdout', extra_norm,
                          expected_stdout_file, actual_stdout_file)

def dump_stdout( name ):
    with open(in_testdir(name, 'run.stdout')) as f:
        str = f.read().strip()
        if str:
            print("Stdout (", name, "):")
            print(str)

def stderr_ok(name, way):
   actual_stderr_file = add_suffix(name, 'run.stderr')
   expected_stderr_file = find_expected_file(name, 'stderr')

   return compare_outputs(way, 'stderr',
                          join_normalisers(normalise_errmsg, getTestOpts().extra_errmsg_normaliser), \
                          expected_stderr_file, actual_stderr_file,
                          whitespace_normaliser=normalise_whitespace)

def dump_stderr( name ):
    with open(in_testdir(name, 'run.stderr')) as f:
        str = f.read().strip()
        if str:
            print("Stderr (", name, "):")
            print(str)

def read_no_crs(file):
    str = ''
    try:
        # See Note [Universal newlines].
        with io.open(file, 'r', encoding='utf8', errors='replace', newline=None) as h:
            str = h.read()
    except Exception:
        # On Windows, if the program fails very early, it seems the
        # files stdout/stderr are redirected to may not get created
        pass
    return str

def write_file(file, str):
    # See Note [Universal newlines].
    with io.open(file, 'w', encoding='utf8', newline='') as h:
        h.write(str)

# Note [Universal newlines]
#
# We don't want to write any Windows style line endings ever, because
# it would mean that `make accept` would touch every line of the file
# when switching between Linux and Windows.
#
# Furthermore, when reading a file, it is convenient to translate all
# Windows style endings to '\n', as it simplifies searching or massaging
# the content.
#
# Solution: use `io.open` instead of `open`
#  * when reading: use newline=None to translate '\r\n' to '\n'
#  * when writing: use newline='' to not translate '\n' to '\r\n'
#
# See https://docs.python.org/2/library/io.html#io.open.
#
# This should work with both python2 and python3, and with both mingw*
# as msys2 style Python.
#
# Do note that io.open returns unicode strings. So we have to specify
# the expected encoding. But there is at least one file which is not
# valid utf8 (decodingerror002.stdout). Solution: use errors='replace'.
# Another solution would be to open files in binary mode always, and
# operate on bytes.

def check_hp_ok(name):
    opts = getTestOpts()

    # do not qualify for hp2ps because we should be in the right directory
    hp2psCmd = 'cd "{opts.testdir}" && {{hp2ps}} {name}'.format(**locals())

    hp2psResult = runCmd(hp2psCmd)

    actual_ps_path = in_testdir(name, 'ps')

    if hp2psResult == 0:
        if os.path.exists(actual_ps_path):
            if gs_working:
                gsResult = runCmd(genGSCmd(actual_ps_path))
                if (gsResult == 0):
                    return (True)
                else:
                    print("hp2ps output for " + name + "is not valid PostScript")
            else: return (True) # assume postscript is valid without ghostscript
        else:
            print("hp2ps did not generate PostScript for " + name)
            return (False)
    else:
        print("hp2ps error when processing heap profile for " + name)
        return(False)

def check_prof_ok(name, way):
    expected_prof_file = find_expected_file(name, 'prof.sample')
    expected_prof_path = in_testdir(expected_prof_file)

    # Check actual prof file only if we have an expected prof file to
    # compare it with.
    if not os.path.exists(expected_prof_path):
        return True

    actual_prof_file = add_suffix(name, 'prof')
    actual_prof_path = in_testdir(actual_prof_file)

    if not os.path.exists(actual_prof_path):
        print(actual_prof_path + " does not exist")
        return(False)

    if os.path.getsize(actual_prof_path) == 0:
        print(actual_prof_path + " is empty")
        return(False)

    return compare_outputs(way, 'prof', normalise_prof,
                            expected_prof_file, actual_prof_file,
                            whitespace_normaliser=normalise_whitespace)

# Compare expected output to actual output, and optionally accept the
# new output. Returns true if output matched or was accepted, false
# otherwise. See Note [Output comparison] for the meaning of the
# normaliser and whitespace_normaliser parameters.
def compare_outputs(way, kind, normaliser, expected_file, actual_file,
                    whitespace_normaliser=lambda x:x):

    expected_path = in_srcdir(expected_file)
    actual_path = in_testdir(actual_file)

    if os.path.exists(expected_path):
        expected_str = normaliser(read_no_crs(expected_path))
        # Create the .normalised file in the testdir, not in the srcdir.
        expected_normalised_file = add_suffix(expected_file, 'normalised')
        expected_normalised_path = in_testdir(expected_normalised_file)
    else:
        expected_str = ''
        expected_normalised_path = '/dev/null'

    actual_raw = read_no_crs(actual_path)
    actual_str = normaliser(actual_raw)

    # See Note [Output comparison].
    if whitespace_normaliser(expected_str) == whitespace_normaliser(actual_str):
        return 1
    else:
        if config.verbose >= 1 and _expect_pass(way):
            print('Actual ' + kind + ' output differs from expected:')

        if expected_normalised_path != '/dev/null':
            write_file(expected_normalised_path, expected_str)

        actual_normalised_path = add_suffix(actual_path, 'normalised')
        write_file(actual_normalised_path, actual_str)

        if config.verbose >= 1 and _expect_pass(way):
            # See Note [Output comparison].
            r = runCmd('diff -uw "{0}" "{1}"'.format(expected_normalised_path,
                                                        actual_normalised_path),
                        print_output = 1)

            # If for some reason there were no non-whitespace differences,
            # then do a full diff
            if r == 0:
                r = runCmd('diff -u "{0}" "{1}"'.format(expected_normalised_path,
                                                           actual_normalised_path),
                           print_output = 1)

        if config.accept and (getTestOpts().expect == 'fail' or
                              way in getTestOpts().expect_fail_for):
            if_verbose(1, 'Test is expected to fail. Not accepting new output.')
            return 0
        elif config.accept and actual_raw:
            if_verbose(1, 'Accepting new output.')
            write_file(expected_path, actual_raw)
            return 1
        elif config.accept:
            if_verbose(1, 'No output. Deleting "{0}".'.format(expected_path))
            os.remove(expected_path)
            return 1
        else:
            return 0

# Note [Output comparison]
#
# We do two types of output comparison:
#
# 1. To decide whether a test has failed. We apply a `normaliser` and an
#    optional `whitespace_normaliser` to the expected and the actual
#    output, before comparing the two.
#
# 2. To show as a diff to the user when the test indeed failed. We apply
#    the same `normaliser` function to the outputs, to make the diff as
#    small as possible (only showing the actual problem). But we don't
#    apply the `whitespace_normaliser` here, because it might completely
#    squash all whitespace, making the diff unreadable. Instead we rely
#    on the `diff` program to ignore whitespace changes as much as
#    possible (#10152).

def normalise_whitespace( str ):
    # Merge contiguous whitespace characters into a single space.
    return ' '.join(w for w in str.split())

callSite_re = re.compile(r', called at (.+):[\d]+:[\d]+ in [\w\-\.]+:')

def normalise_callstacks(s):
    opts = getTestOpts()
    def repl(matches):
        location = matches.group(1)
        location = normalise_slashes_(location)
        return ', called at {0}:<line>:<column> in <package-id>:'.format(location)
    # Ignore line number differences in call stacks (#10834).
    s = re.sub(callSite_re, repl, s)
    # Ignore the change in how we identify implicit call-stacks
    s = s.replace('from ImplicitParams', 'from HasCallStack')
    if not opts.keep_prof_callstacks:
        # Don't output prof callstacks. Test output should be
        # independent from the WAY we run the test.
        s = re.sub(r'CallStack \(from -prof\):(\n  .*)*\n?', '', s)
    return s

tyCon_re = re.compile(r'TyCon\s*\d+L?\#\#\s*\d+L?\#\#\s*', flags=re.MULTILINE)

def normalise_type_reps(str):
    """ Normalise out fingerprints from Typeable TyCon representations """
    return re.sub(tyCon_re, 'TyCon FINGERPRINT FINGERPRINT ', str)

def normalise_errmsg( str ):
    """Normalise error-messages emitted via stderr"""
    # IBM AIX's `ld` is a bit chatty
    if opsys('aix'):
        str = str.replace('ld: 0706-027 The -x flag is ignored.\n', '')
    # remove " error:" and lower-case " Warning:" to make patch for
    # trac issue #10021 smaller
    str = modify_lines(str, lambda l: re.sub(' error:', '', l))
    str = modify_lines(str, lambda l: re.sub(' Warning:', ' warning:', l))
    str = normalise_callstacks(str)
    str = normalise_type_reps(str)

    # If somefile ends in ".exe" or ".exe:", zap ".exe" (for Windows)
    #    the colon is there because it appears in error messages; this
    #    hacky solution is used in place of more sophisticated filename
    #    mangling
    str = re.sub('([^\\s])\\.exe', '\\1', str)

    # normalise slashes, minimise Windows/Unix filename differences
    str = re.sub('\\\\', '/', str)

    # The inplace ghc's are called ghc-stage[123] to avoid filename
    # collisions, so we need to normalise that to just "ghc"
    str = re.sub('ghc-stage[123]', 'ghc', str)

    # Error messages simetimes contain integer implementation package
    str = re.sub('integer-(gmp|simple)-[0-9.]+', 'integer-<IMPL>-<VERSION>', str)

    # Also filter out bullet characters.  This is because bullets are used to
    # separate error sections, and tests shouldn't be sensitive to how the
    # the division happens.
    bullet = '•'.encode('utf8') if isinstance(str, bytes) else '•'
    str = str.replace(bullet, '')

    # Windows only, this is a bug in hsc2hs but it is preventing
    # stable output for the testsuite. See Trac #9775. For now we filter out this
    # warning message to get clean output.
    if config.msys:
        str = re.sub('Failed to remove file (.*); error= (.*)$', '', str)
        str = re.sub('DeleteFile "(.+)": permission denied \(Access is denied\.\)(.*)$', '', str)

    return str

# normalise a .prof file, so that we can reasonably compare it against
# a sample.  This doesn't compare any of the actual profiling data,
# only the shape of the profile and the number of entries.
def normalise_prof (str):
    # strip everything up to the line beginning "COST CENTRE"
    str = re.sub('^(.*\n)*COST CENTRE[^\n]*\n','',str)

    # strip results for CAFs, these tend to change unpredictably
    str = re.sub('[ \t]*(CAF|IDLE).*\n','',str)

    # XXX Ignore Main.main.  Sometimes this appears under CAF, and
    # sometimes under MAIN.
    str = re.sub('[ \t]*main[ \t]+Main.*\n','',str)

    # We have something like this:
    #
    # MAIN         MAIN  <built-in>                 53  0  0.0   0.2  0.0  100.0
    #  CAF         Main  <entire-module>           105  0  0.0   0.3  0.0   62.5
    #   readPrec   Main  Main_1.hs:7:13-16         109  1  0.0   0.6  0.0    0.6
    #   readPrec   Main  Main_1.hs:4:13-16         107  1  0.0   0.6  0.0    0.6
    #   main       Main  Main_1.hs:(10,1)-(20,20)  106  1  0.0  20.2  0.0   61.0
    #    ==        Main  Main_1.hs:7:25-26         114  1  0.0   0.0  0.0    0.0
    #    ==        Main  Main_1.hs:4:25-26         113  1  0.0   0.0  0.0    0.0
    #    showsPrec Main  Main_1.hs:7:19-22         112  2  0.0   1.2  0.0    1.2
    #    showsPrec Main  Main_1.hs:4:19-22         111  2  0.0   0.9  0.0    0.9
    #    readPrec  Main  Main_1.hs:7:13-16         110  0  0.0  18.8  0.0   18.8
    #    readPrec  Main  Main_1.hs:4:13-16         108  0  0.0  19.9  0.0   19.9
    #
    # then we remove all the specific profiling data, leaving only the cost
    # centre name, module, src, and entries, to end up with this: (modulo
    # whitespace between columns)
    #
    # MAIN      MAIN <built-in>         0
    # readPrec  Main Main_1.hs:7:13-16  1
    # readPrec  Main Main_1.hs:4:13-16  1
    # ==        Main Main_1.hs:7:25-26  1
    # ==        Main Main_1.hs:4:25-26  1
    # showsPrec Main Main_1.hs:7:19-22  2
    # showsPrec Main Main_1.hs:4:19-22  2
    # readPrec  Main Main_1.hs:7:13-16  0
    # readPrec  Main Main_1.hs:4:13-16  0

    # Split 9 whitespace-separated groups, take columns 1 (cost-centre), 2
    # (module), 3 (src), and 5 (entries). SCC names can't have whitespace, so
    # this works fine.
    str = re.sub(r'\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*',
            '\\1 \\2 \\3 \\5\n', str)
    return str

def normalise_slashes_( str ):
    str = re.sub('\\\\', '/', str)
    return str

def normalise_exe_( str ):
    str = re.sub('\.exe', '', str)
    return str

def normalise_output( str ):
    # remove " error:" and lower-case " Warning:" to make patch for
    # trac issue #10021 smaller
    str = modify_lines(str, lambda l: re.sub(' error:', '', l))
    str = modify_lines(str, lambda l: re.sub(' Warning:', ' warning:', l))
    # Remove a .exe extension (for Windows)
    # This can occur in error messages generated by the program.
    str = re.sub('([^\\s])\\.exe', '\\1', str)
    str = normalise_callstacks(str)
    str = normalise_type_reps(str)
    return str

def normalise_asm( str ):
    lines = str.split('\n')
    # Only keep instructions and labels not starting with a dot.
    metadata = re.compile('^[ \t]*\\..*$')
    out = []
    for line in lines:
      # Drop metadata directives (e.g. ".type")
      if not metadata.match(line):
        line = re.sub('@plt', '', line)
        instr = line.lstrip().split()
        # Drop empty lines.
        if not instr:
          continue
        # Drop operands, except for call instructions.
        elif instr[0] == 'call':
          out.append(instr[0] + ' ' + instr[1])
        else:
          out.append(instr[0])
    out = '\n'.join(out)
    return out

def if_verbose( n, s ):
    if config.verbose >= n:
        print(s)

def if_verbose_dump( n, f ):
    if config.verbose >= n:
        try:
            with io.open(f) as file:
                print(file.read())
        except Exception:
            print('')

def runCmd(cmd, stdin=None, stdout=None, stderr=None, timeout_multiplier=1.0, print_output=0):
    timeout_prog = strip_quotes(config.timeout_prog)
    timeout = str(int(ceil(config.timeout * timeout_multiplier)))

    # Format cmd using config. Example: cmd='{hpc} report A.tix'
    cmd = cmd.format(**config.__dict__)
    if_verbose(3, cmd + ('< ' + os.path.basename(stdin) if stdin else ''))

    # declare the buffers to a default
    stdin_buffer  = None

    stdin_file = io.open(stdin, 'rb') if stdin else None
    stdout_buffer = b''
    stderr_buffer = b''

    hStdErr = subprocess.PIPE
    if stderr is subprocess.STDOUT:
        hStdErr = subprocess.STDOUT

    try:
        # cmd is a complex command in Bourne-shell syntax
        # e.g (cd . && 'C:/users/simonpj/HEAD/inplace/bin/ghc-stage2' ...etc)
        # Hence it must ultimately be run by a Bourne shell. It's timeout's job
        # to invoke the Bourne shell

        r = subprocess.Popen([timeout_prog, timeout, cmd],
                             stdin=stdin_file,
                             stdout=subprocess.PIPE,
                             stderr=hStdErr)

        stdout_buffer, stderr_buffer = r.communicate()
    finally:
        if stdin_file:
            stdin_file.close()
        if config.verbose >= 1 and print_output >= 1:
            if stdout_buffer:
                sys.stdout.buffer.write(stdout_buffer)
            if stderr_buffer:
                sys.stderr.buffer.write(stderr_buffer)

        if stdout:
            with io.open(stdout, 'wb') as f:
                f.write(stdout_buffer)
        if stderr:
            if stderr is not subprocess.STDOUT:
                with io.open(stderr, 'wb') as f:
                    f.write(stderr_buffer)

    if r.returncode == 98:
        # The python timeout program uses 98 to signal that ^C was pressed
        stopNow()
    if r.returncode == 99 and getTestOpts().exit_code != 99:
        # Only print a message when timeout killed the process unexpectedly.
        if_verbose(1, 'Timeout happened...killed process "{0}"...\n'.format(cmd))
    return r.returncode

# -----------------------------------------------------------------------------
# checking if ghostscript is available for checking the output of hp2ps

def genGSCmd(psfile):
    return '{{gs}} -dNODISPLAY -dBATCH -dQUIET -dNOPAUSE "{0}"'.format(psfile)

def gsNotWorking():
    global gs_working
    print("GhostScript not available for hp2ps tests")

global gs_working
gs_working = 0
if config.have_profiling:
  if config.gs != '':
    resultGood = runCmd(genGSCmd(config.confdir + '/good.ps'));
    if resultGood == 0:
        resultBad = runCmd(genGSCmd(config.confdir + '/bad.ps') +
                                   ' >/dev/null 2>&1')
        if resultBad != 0:
            print("GhostScript available for hp2ps tests")
            gs_working = 1;
        else:
            gsNotWorking();
    else:
        gsNotWorking();
  else:
    gsNotWorking();

def add_suffix( name, suffix ):
    if suffix == '':
        return name
    else:
        return name + '.' + suffix

def add_hs_lhs_suffix(name):
    if getTestOpts().c_src:
        return add_suffix(name, 'c')
    elif getTestOpts().cmm_src:
        return add_suffix(name, 'cmm')
    elif getTestOpts().objc_src:
        return add_suffix(name, 'm')
    elif getTestOpts().objcpp_src:
        return add_suffix(name, 'mm')
    elif getTestOpts().literate:
        return add_suffix(name, 'lhs')
    else:
        return add_suffix(name, 'hs')

def replace_suffix( name, suffix ):
    base, suf = os.path.splitext(name)
    return base + '.' + suffix

def in_testdir(name, suffix=''):
    return os.path.join(getTestOpts().testdir, add_suffix(name, suffix))

def in_srcdir(name, suffix=''):
    return os.path.join(getTestOpts().srcdir, add_suffix(name, suffix))

# Finding the sample output.  The filename is of the form
#
#   <test>.stdout[-ws-<wordsize>][-<platform>]
#
def find_expected_file(name, suff):
    basename = add_suffix(name, suff)

    files = [basename + ws + plat
             for plat in ['-' + config.platform, '-' + config.os, '']
             for ws in ['-ws-' + config.wordsize, '']]

    for f in files:
        if os.path.exists(in_srcdir(f)):
            return f

    return basename

if config.msys:
    import stat
    import time
    def cleanup():
        testdir = getTestOpts().testdir
        max_attempts = 5
        retries = max_attempts
        def on_error(function, path, excinfo):
            # At least one test (T11489) removes the write bit from a file it
            # produces. Windows refuses to delete read-only files with a
            # permission error. Try setting the write bit and try again.
            os.chmod(path, stat.S_IWRITE)
            function(path)

        # On Windows we have to retry the delete a couple of times.
        # The reason for this is that a FileDelete command just marks a
        # file for deletion. The file is really only removed when the last
        # handle to the file is closed. Unfortunately there are a lot of
        # system services that can have a file temporarily opened using a shared
        # readonly lock, such as the built in AV and search indexer.
        #
        # We can't really guarantee that these are all off, so what we can do is
        # whenever after a rmtree the folder still exists to try again and wait a bit.
        #
        # Based on what I've seen from the tests on CI server, is that this is relatively rare.
        # So overall we won't be retrying a lot. If after a reasonable amount of time the folder is
        # still locked then abort the current test by throwing an exception, this so it won't fail
        # with an even more cryptic error.
        #
        # See Trac #13162
        exception = None
        while retries > 0 and os.path.exists(testdir):
            time.sleep((max_attempts-retries)*6)
            try:
                shutil.rmtree(testdir, onerror=on_error, ignore_errors=False)
            except Exception as e:
                exception = e
            retries -= 1

        if retries == 0 and os.path.exists(testdir):
            raise Exception("Unable to remove folder '%s': %s\nUnable to start current test."
                            % (testdir, exception))
else:
    def cleanup():
        testdir = getTestOpts().testdir
        if os.path.exists(testdir):
            shutil.rmtree(testdir, ignore_errors=False)


# -----------------------------------------------------------------------------
# Return a list of all the files ending in '.T' below directories roots.

def findTFiles(roots):
    for root in roots:
        for path, dirs, files in os.walk(root, topdown=True):
            # Never pick up .T files in uncleaned .run directories.
            dirs[:] = [dir for dir in sorted(dirs)  
                           if not dir.endswith(testdir_suffix)]
            for filename in files:
                if filename.endswith('.T'):
                    yield os.path.join(path, filename)

# -----------------------------------------------------------------------------
# Output a test summary to the specified file object

def summary(t, file, short=False):

    file.write('\n')
    printUnexpectedTests(file,
        [t.unexpected_passes, t.unexpected_failures,
         t.unexpected_stat_failures, t.framework_failures])

    if short:
        # Only print the list of unexpected tests above.
        return

    file.write('SUMMARY for test run started at '
               + time.strftime("%c %Z", t.start_time) + '\n'
               + str(datetime.timedelta(seconds=
                    round(time.time() - time.mktime(t.start_time)))).rjust(8)
               + ' spent to go through\n'
               + repr(t.total_tests).rjust(8)
               + ' total tests, which gave rise to\n'
               + repr(t.total_test_cases).rjust(8)
               + ' test cases, of which\n'
               + repr(t.n_tests_skipped).rjust(8)
               + ' were skipped\n'
               + '\n'
               + repr(len(t.missing_libs)).rjust(8)
               + ' had missing libraries\n'
               + repr(t.n_expected_passes).rjust(8)
               + ' expected passes\n'
               + repr(t.n_expected_failures).rjust(8)
               + ' expected failures\n'
               + '\n'
               + repr(len(t.framework_failures)).rjust(8)
               + ' caused framework failures\n'
               + repr(len(t.framework_warnings)).rjust(8)
               + ' caused framework warnings\n'
               + repr(len(t.unexpected_passes)).rjust(8)
               + ' unexpected passes\n'
               + repr(len(t.unexpected_failures)).rjust(8)
               + ' unexpected failures\n'
               + repr(len(t.unexpected_stat_failures)).rjust(8)
               + ' unexpected stat failures\n'
               + '\n')

    if t.unexpected_passes:
        file.write('Unexpected passes:\n')
        printTestInfosSummary(file, t.unexpected_passes)

    if t.unexpected_failures:
        file.write('Unexpected failures:\n')
        printTestInfosSummary(file, t.unexpected_failures)

    if t.unexpected_stat_failures:
        file.write('Unexpected stat failures:\n')
        printTestInfosSummary(file, t.unexpected_stat_failures)

    if t.framework_failures:
        file.write('Framework failures:\n')
        printTestInfosSummary(file, t.framework_failures)

    if t.framework_warnings:
        file.write('Framework warnings:\n')
        printTestInfosSummary(file, t.framework_warnings)

    if stopping():
        file.write('WARNING: Testsuite run was terminated early\n')

def printUnexpectedTests(file, testInfoss):
    unexpected = set(name for testInfos in testInfoss
                       for (_, name, _, _) in testInfos
                       if not name.endswith('.T'))
    if unexpected:
        file.write('Unexpected results from:\n')
        file.write('TEST="' + ' '.join(sorted(unexpected)) + '"\n')
        file.write('\n')

def printTestInfosSummary(file, testInfos):
    maxDirLen = max(len(directory) for (directory, _, _, _) in testInfos)
    for (directory, name, reason, way) in testInfos:
        directory = directory.ljust(maxDirLen)
        file.write('   {directory}  {name} [{reason}] ({way})\n'.format(**locals()))
    file.write('\n')

def modify_lines(s, f):
    s = '\n'.join([f(l) for l in s.splitlines()])
    if s and s[-1] != '\n':
        # Prevent '\ No newline at end of file' warnings when diffing.
        s += '\n'
    return s
