#
# (c) Simon Marlow 2002
#

from __future__ import print_function

import shutil
import sys
import os
import errno
import string
import re
import traceback
import time
import datetime
import copy
import glob
from math import ceil, trunc
import collections
import subprocess

from testglobals import *
from testutil import *

if config.use_threads:
    import threading
    try:
        import thread
    except ImportError: # Python 3
        import _thread as thread

global wantToStop
wantToStop = False
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
    return len(opts.compiler_stats_range_fields) > 0 or len(opts.stats_range_fields) > 0


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

def ignore_output( name, opts ):
    opts.ignore_output = 1

def no_stdin( name, opts ):
    opts.no_stdin = 1

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

def only_compiler_types( _compiler_types ):
   # Don't delete yet. The libraries unix, stm and hpc still call this function.
   return lambda _name, _opts: None 

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
    assert not isinstance(files, str), files
    return lambda name, opts, v=files: _extra_clean(name, opts, v);

def _extra_clean( name, opts, v ):
    opts.clean_files = v

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
        framework_fail(name, 'numfield-no-expected', 'No expected value found for ' + field + ' in num_field check')

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

    framework_fail(name, 'numfield-no-expected', 'No expected value found for ' + field + ' in num_field check')

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

def ghci_dynamic( ):
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

def compiler_lt( compiler, version ):
    assert compiler == 'ghc'
    return version_lt(config.compiler_version, version)

def compiler_le( compiler, version ):
    assert compiler == 'ghc'
    return version_le(config.compiler_version, version)

def compiler_gt( compiler, version ):
    assert compiler == 'ghc'
    return version_gt(config.compiler_version, version)

def compiler_ge( compiler, version ):
    assert compiler == 'ghc'
    return version_ge(config.compiler_version, version)

def unregisterised( ):
    return config.unregisterised

def compiler_profiled( ):
    return config.compiler_profiled

def compiler_debugged( ):
    return config.compiler_debugged

def tag( t ):
    return t in config.compiler_tags

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
    return lambda name, opts, c=cmd: _clean_cmd(name, opts, cmd)

def _clean_cmd( name, opts, cmd ):
    opts.clean_cmd = cmd

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

# ----

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
            if isinstance(el, collections.Iterable) and not isinstance(el, basestring):
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

def newTestDir( dir ):
    global thisdir_settings
    # reset the options for this test directory
    thisdir_settings = lambda name, opts, dir=dir: _newTestDir( name, opts, dir )

def _newTestDir( name, opts, dir ):
    opts.testdir = dir
    opts.compiler_always_flags = config.compiler_always_flags

# -----------------------------------------------------------------------------
# Actually doing tests

parallelTests = []
aloneTests = []
allTestNames = set([])

def runTest (opts, name, func, args):
    ok = 0

    if config.use_threads:
        t.thread_pool.acquire()
        try:
            while config.threads<(t.running_threads+1):
                t.thread_pool.wait()
            t.running_threads = t.running_threads+1
            ok=1
            t.thread_pool.release()
            thread.start_new_thread(test_common_thread, (name, opts, func, args))
        except:
            if not ok:
                t.thread_pool.release()
    else:
        test_common_work (name, opts, func, args)

# name  :: String
# setup :: TestOpts -> IO ()
def test (name, setup, func, args):
    if config.only and name not in config.only:
        return

    global aloneTests
    global parallelTests
    global allTestNames
    global thisdir_settings
    if name in allTestNames:
        framework_fail(name, 'duplicate', 'There are multiple tests with this name')
    if not re.match('^[0-9]*[a-zA-Z][a-zA-Z0-9._-]*$', name):
        framework_fail(name, 'bad_name', 'This test has an invalid name')

    # Make a deep copy of the default_testopts, as we need our own copy
    # of any dictionaries etc inside it. Otherwise, if one test modifies
    # them, all tests will see the modified version!
    myTestOpts = copy.deepcopy(default_testopts)

    executeSetups([thisdir_settings, setup], name, myTestOpts)

    thisTest = lambda : runTest(myTestOpts, name, func, args)
    if myTestOpts.alone:
        aloneTests.append(thisTest)
    else:
        parallelTests.append(thisTest)
    allTestNames.add(name)

if config.use_threads:
    def test_common_thread(name, opts, func, args):
        t.lock.acquire()
        try:
            test_common_work(name,opts,func,args)
        finally:
            t.lock.release()
            t.thread_pool.acquire()
            t.running_threads = t.running_threads - 1
            t.thread_pool.notify()
            t.thread_pool.release()

def get_package_cache_timestamp():
    if config.package_conf_cache_file == '':
        return 0.0
    else:
        try:
            return os.stat(config.package_conf_cache_file).st_mtime
        except:
            return 0.0


def test_common_work (name, opts, func, args):
    try:
        t.total_tests = t.total_tests+1
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

        t.total_test_cases = t.total_test_cases + len(all_ways)

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
        if config.speed > 0:
            do_ways = do_ways[:1]

        if not config.clean_only:
            # Run the required tests...
            for way in do_ways:
                if stopping():
                    break
                do_test (name, way, func, args)

            for way in all_ways:
                if way not in do_ways:
                    skiptest (name,way)

        if getTestOpts().cleanup != '' and (config.clean_only or do_ways != []):
            pretest_cleanup(name)
            clean([name + suff for suff in [
                       '', '.exe', '.exe.manifest', '.genscript',
                       '.stderr.normalised',        '.stdout.normalised',
                       '.run.stderr.normalised',    '.run.stdout.normalised',
                       '.comp.stderr.normalised',   '.comp.stdout.normalised',
                       '.interp.stderr.normalised', '.interp.stdout.normalised',
                       '.stats', '.comp.stats',
                       '.hi', '.o', '.prof', '.exe.prof', '.hc',
                       '_stub.h', '_stub.c', '_stub.o',
                       '.hp', '.exe.hp', '.ps', '.aux', '.hcr', '.eventlog']])

            if func == multi_compile or func == multi_compile_fail:
                    extra_mods = args[1]
                    clean([replace_suffix(fx[0],'o') for fx in extra_mods])
                    clean([replace_suffix(fx[0], 'hi') for fx in extra_mods])


            clean(getTestOpts().clean_files)

            if getTestOpts().outputdir != None:
                odir = in_testdir(getTestOpts().outputdir)
                try:
                    shutil.rmtree(odir)
                except:
                    pass

            try:
                shutil.rmtree(in_testdir('.hpc.' + name))
            except:
                pass

            try:
                cleanCmd = getTestOpts().clean_cmd
                if cleanCmd != None:
                    result = runCmdFor(name, 'cd ' + getTestOpts().testdir + ' && ' + cleanCmd)
                    if result != 0:
                        framework_fail(name, 'cleaning', 'clean-command failed: ' + str(result))
            except:
                framework_fail(name, 'cleaning', 'clean-command exception')

        package_conf_cache_file_end_timestamp = get_package_cache_timestamp();

        if package_conf_cache_file_start_timestamp != package_conf_cache_file_end_timestamp:
            framework_fail(name, 'whole-test', 'Package cache timestamps do not match: ' + str(package_conf_cache_file_start_timestamp) + ' ' + str(package_conf_cache_file_end_timestamp))

        try:
            for f in files_written[name]:
                if os.path.exists(f):
                    try:
                        if not f in files_written_not_removed[name]:
                            files_written_not_removed[name].append(f)
                    except:
                        files_written_not_removed[name] = [f]
        except:
            pass
    except Exception as e:
        framework_fail(name, 'runTest', 'Unhandled exception: ' + str(e))

def clean(strs):
    for str in strs:
        if (str.endswith('.package.conf') or
            str.startswith('package.conf.') and not str.endswith('/*')):
            # Package confs are directories now.
            str += '/*'

        for name in glob.glob(in_testdir(str)):
            clean_full_path(name)

def clean_full_path(name):
        try:
            # Remove files...
            os.remove(name)
        except OSError as e1:
            try:
                # ... and empty directories
                os.rmdir(name)
            except OSError as e2:
                # We don't want to fail here, but we do want to know
                # what went wrong, so print out the exceptions.
                # ENOENT isn't a problem, though, as we clean files
                # that don't necessarily exist.
                if e1.errno != errno.ENOENT:
                    print(e1)
                if e2.errno != errno.ENOENT:
                    print(e2)

def do_test(name, way, func, args):
    full_name = name + '(' + way + ')'

    try:
        if_verbose(2, "=====> %s %d of %d %s " % \
                    (full_name, t.total_tests, len(allTestNames), \
                    [t.n_unexpected_passes, \
                     t.n_unexpected_failures, \
                     t.n_framework_failures]))

        if config.use_threads:
            t.lock.release()

        try:
            preCmd = getTestOpts().pre_cmd
            if preCmd != None:
                result = runCmdFor(name, 'cd ' + getTestOpts().testdir + ' && ' + preCmd)
                if result != 0:
                    framework_fail(name, way, 'pre-command failed: ' + str(result))
        except:
            framework_fail(name, way, 'pre-command exception')

        try:
            result = func(*[name,way] + args)
        finally:
            if config.use_threads:
                t.lock.acquire()

        if getTestOpts().expect != 'pass' and \
                getTestOpts().expect != 'fail' and \
                getTestOpts().expect != 'missing-lib':
            framework_fail(name, way, 'bad expected ' + getTestOpts().expect)

        try:
            passFail = result['passFail']
        except:
            passFail = 'No passFail found'

        if passFail == 'pass':
            if _expect_pass(way):
                t.n_expected_passes = t.n_expected_passes + 1
                if name in t.expected_passes:
                    t.expected_passes[name].append(way)
                else:
                    t.expected_passes[name] = [way]
            else:
                if_verbose(1, '*** unexpected pass for %s' % full_name)
                t.n_unexpected_passes = t.n_unexpected_passes + 1
                addPassingTestInfo(t.unexpected_passes, getTestOpts().testdir, name, way)
        elif passFail == 'fail':
            if _expect_pass(way):
                reason = result['reason']
                tag = result.get('tag')
                if tag == 'stat':
                    if_verbose(1, '*** unexpected stat test failure for %s' % full_name)
                    t.n_unexpected_stat_failures = t.n_unexpected_stat_failures + 1
                    addFailingTestInfo(t.unexpected_stat_failures, getTestOpts().testdir, name, reason, way)
                else:
                    if_verbose(1, '*** unexpected failure for %s' % full_name)
                    t.n_unexpected_failures = t.n_unexpected_failures + 1
                    addFailingTestInfo(t.unexpected_failures, getTestOpts().testdir, name, reason, way)
            else:
                if getTestOpts().expect == 'missing-lib':
                    t.n_missing_libs = t.n_missing_libs + 1
                    if name in t.missing_libs:
                        t.missing_libs[name].append(way)
                    else:
                        t.missing_libs[name] = [way]
                else:
                    t.n_expected_failures = t.n_expected_failures + 1
                    if name in t.expected_failures:
                        t.expected_failures[name].append(way)
                    else:
                        t.expected_failures[name] = [way]
        else:
            framework_fail(name, way, 'bad result ' + passFail)
    except KeyboardInterrupt:
        stopNow()
    except:
        framework_fail(name, way, 'do_test exception')
        traceback.print_exc()

def addPassingTestInfo (testInfos, directory, name, way):
    directory = re.sub('^\\.[/\\\\]', '', directory)

    if not directory in testInfos:
        testInfos[directory] = {}

    if not name in testInfos[directory]:
        testInfos[directory][name] = []

    testInfos[directory][name].append(way)

def addFailingTestInfo (testInfos, directory, name, reason, way):
    directory = re.sub('^\\.[/\\\\]', '', directory)

    if not directory in testInfos:
        testInfos[directory] = {}

    if not name in testInfos[directory]:
        testInfos[directory][name] = {}

    if not reason in testInfos[directory][name]:
        testInfos[directory][name][reason] = []

    testInfos[directory][name][reason].append(way)

def skiptest (name, way):
    # print 'Skipping test \"', name, '\"'
    t.n_tests_skipped = t.n_tests_skipped + 1
    if name in t.tests_skipped:
        t.tests_skipped[name].append(way)
    else:
        t.tests_skipped[name] = [way]

def framework_fail( name, way, reason ):
    full_name = name + '(' + way + ')'
    if_verbose(1, '*** framework failure for %s %s ' % (full_name, reason))
    t.n_framework_failures = t.n_framework_failures + 1
    if name in t.framework_failures:
        t.framework_failures[name].append(way)
    else:
        t.framework_failures[name] = [way]

def badResult(result):
    try:
        if result['passFail'] == 'pass':
            return False
        return True
    except:
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
# altogether by using run_command_ignore_output instead of
# run_command.

def run_command( name, way, cmd ):
    return simple_run( name, '', cmd, '' )

# -----------------------------------------------------------------------------
# GHCi tests

def ghci_script_without_flag(flag):
    def apply(name, way, script):
        overrides = [f for f in getTestOpts().compiler_always_flags if f != flag]
        return ghci_script_override_default_flags(overrides)(name, way, script)

    return apply

def ghci_script_override_default_flags(overrides):
    def apply(name, way, script):
        return ghci_script(name, way, script, overrides)

    return apply

def ghci_script( name, way, script, override_flags = None ):
    # filter out -fforce-recomp from compiler_always_flags, because we're
    # actually testing the recompilation behaviour in the GHCi tests.
    flags = ' '.join(get_compiler_flags(override_flags, noforce=True))

    way_flags = ' '.join(config.way_flags(name)['ghci'])

    # We pass HC and HC_OPTS as environment variables, so that the
    # script can invoke the correct compiler by using ':! $HC $HC_OPTS'
    cmd = ('HC={{compiler}} HC_OPTS="{flags}" {{compiler}} {flags} {way_flags}'
          ).format(flags=flags, way_flags=way_flags)

    getTestOpts().stdin = script
    return simple_run( name, way, cmd, getTestOpts().extra_run_opts )

# -----------------------------------------------------------------------------
# Compile-only tests

def compile_override_default_flags(overrides):
    def apply(name, way, extra_opts):
        return do_compile(name, way, 0, '', [], extra_opts, overrides)

    return apply

def compile_fail_override_default_flags(overrides):
    def apply(name, way, extra_opts):
        return do_compile(name, way, 1, '', [], extra_opts, overrides)

    return apply

def compile_without_flag(flag):
    def apply(name, way, extra_opts):
        overrides = [f for f in getTestOpts().compiler_always_flags if f != flag]
        return compile_override_default_flags(overrides)(name, way, extra_opts)

    return apply

def compile_fail_without_flag(flag):
    def apply(name, way, extra_opts):
        overrides = [f for f in getTestOpts.compiler_always_flags if f != flag]
        return compile_fail_override_default_flags(overrides)(name, way, extra_opts)

    return apply

def compile( name, way, extra_hc_opts ):
    return do_compile( name, way, 0, '', [], extra_hc_opts )

def compile_fail( name, way, extra_hc_opts ):
    return do_compile( name, way, 1, '', [], extra_hc_opts )

def multimod_compile( name, way, top_mod, extra_hc_opts ):
    return do_compile( name, way, 0, top_mod, [], extra_hc_opts )

def multimod_compile_fail( name, way, top_mod, extra_hc_opts ):
    return do_compile( name, way, 1, top_mod, [], extra_hc_opts )

def multi_compile( name, way, top_mod, extra_mods, extra_hc_opts ):
    return do_compile( name, way, 0, top_mod, extra_mods, extra_hc_opts)

def multi_compile_fail( name, way, top_mod, extra_mods, extra_hc_opts ):
    return do_compile( name, way, 1, top_mod, extra_mods, extra_hc_opts)

def do_compile( name, way, should_fail, top_mod, extra_mods, extra_hc_opts, override_flags = None ):
    # print 'Compile only, extra args = ', extra_hc_opts
    pretest_cleanup(name)

    result = extras_build( way, extra_mods, extra_hc_opts )
    if badResult(result):
       return result
    extra_hc_opts = result['hc_opts']

    force = 0
    if extra_mods:
       force = 1
    result = simple_build( name, way, extra_hc_opts, should_fail, top_mod, 0, 1, force, override_flags )

    if badResult(result):
        return result

    # the actual stderr should always match the expected, regardless
    # of whether we expected the compilation to fail or not (successful
    # compilations may generate warnings).

    (_, expected_stderr_file) = find_expected_file(name, 'stderr')
    actual_stderr_file = add_suffix(name, 'comp.stderr')

    if not compare_outputs(way, 'stderr',
                           join_normalisers(getTestOpts().extra_errmsg_normaliser,
                                            normalise_errmsg),
                           expected_stderr_file, actual_stderr_file,
                           whitespace_normaliser=normalise_whitespace):
        return failBecause('stderr mismatch')

    # no problems found, this test passed
    return passed()

def compile_cmp_asm( name, way, extra_hc_opts ):
    print('Compile only, extra args = ', extra_hc_opts)
    pretest_cleanup(name)
    result = simple_build( name + '.cmm', way, '-keep-s-files -O ' + extra_hc_opts, 0, '', 0, 0, 0)

    if badResult(result):
        return result

    # the actual stderr should always match the expected, regardless
    # of whether we expected the compilation to fail or not (successful
    # compilations may generate warnings).

    (_, expected_asm_file) = find_expected_file(name, 'asm')
    actual_asm_file = add_suffix(name, 's')

    if not compare_outputs(way, 'asm',
                           join_normalisers(normalise_errmsg, normalise_asm),
                           expected_asm_file, actual_asm_file):
        return failBecause('asm mismatch')

    # no problems found, this test passed
    return passed()

# -----------------------------------------------------------------------------
# Compile-and-run tests

def compile_and_run__( name, way, top_mod, extra_mods, extra_hc_opts ):
    # print 'Compile and run, extra args = ', extra_hc_opts
    pretest_cleanup(name)

    result = extras_build( way, extra_mods, extra_hc_opts )
    if badResult(result):
       return result
    extra_hc_opts = result['hc_opts']

    if way == 'ghci': # interpreted...
        return interpreter_run( name, way, extra_hc_opts, 0, top_mod )
    else: # compiled...
        force = 0
        if extra_mods:
           force = 1

        result = simple_build( name, way, extra_hc_opts, 0, top_mod, 1, 1, force)
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
    if len(range_fields) > 0:
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
                valStr = str(val)
                valLen = len(valStr)
                expectedStr = str(expected)
                expectedLen = len(expectedStr)
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
    for modopts in extra_mods:
        mod, opts = modopts
        result = simple_build( mod, way, opts + ' ' + extra_hc_opts, 0, '', 0, 0, 0)
        if not (mod.endswith('.hs') or mod.endswith('.lhs')):
            extra_hc_opts += ' ' + replace_suffix(mod, 'o')
        if badResult(result):
            return result

    return {'passFail' : 'pass', 'hc_opts' : extra_hc_opts}


def simple_build( name, way, extra_hc_opts, should_fail, top_mod, link, addsuf, noforce, override_flags = None ):
    opts = getTestOpts()
    errname = add_suffix(name, 'comp.stderr')
    rm_no_fail( qualify(errname, '') )

    if top_mod != '':
        srcname = top_mod
        rm_no_fail( qualify(name, '') )
        base, suf = os.path.splitext(top_mod)
        rm_no_fail( qualify(base, '') )
        rm_no_fail( qualify(base, 'exe') )
    elif addsuf:
        srcname = add_hs_lhs_suffix(name)
        rm_no_fail( qualify(name, '') )
    else:
        srcname = name
        rm_no_fail( qualify(name, 'o') )

    rm_no_fail( qualify(replace_suffix(srcname, "o"), '') )

    to_do = ''
    if top_mod != '':
        to_do = '--make '
        if link:
            to_do = to_do + '-o ' + name
    elif link:
        to_do = '-o ' + name
    elif opts.compile_to_hc:
        to_do = '-C'
    else:
        to_do = '-c' # just compile

    stats_file = name + '.comp.stats'
    if len(opts.compiler_stats_range_fields) > 0:
        extra_hc_opts += ' +RTS -V0 -t' + stats_file + ' --machine-readable -RTS'

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

    flags = ' '.join(get_compiler_flags(override_flags, noforce) +
                     config.way_flags(name)[way])

    cmd = ('cd {opts.testdir} && {cmd_prefix} '
           '{{compiler}} {to_do} {srcname} {flags} {extra_hc_opts} '
           '> {errname} 2>&1'
          ).format(**locals())

    result = runCmdFor(name, cmd, timeout_multiplier=opts.compile_timeout_multiplier)

    if result != 0 and not should_fail:
        if config.verbose >= 1 and _expect_pass(way):
            print('Compile failed (status ' + repr(result) + ') errors were:')
            actual_stderr_path = in_testdir(name, 'comp.stderr')
            if_verbose_dump(1, actual_stderr_path)

    # ToDo: if the sub-shell was killed by ^C, then exit

    statsResult = checkStats(name, way, stats_file, opts.compiler_stats_range_fields)

    if badResult(statsResult):
        return statsResult

    if should_fail:
        if result == 0:
            return failBecause('exit code 0')
    else:
        if result != 0:
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
    if opts.stdin != '':
        use_stdin = opts.stdin
    else:
        stdin_file = add_suffix(name, 'stdin')
        if os.path.exists(in_testdir(stdin_file)):
            use_stdin = stdin_file
        else:
            use_stdin = '/dev/null'

    run_stdout = add_suffix(name,'run.stdout')
    run_stderr = add_suffix(name,'run.stderr')

    rm_no_fail(qualify(name,'run.stdout'))
    rm_no_fail(qualify(name,'run.stderr'))
    rm_no_fail(qualify(name, 'hp'))
    rm_no_fail(qualify(name,'ps'))
    rm_no_fail(qualify(name, 'prof'))

    my_rts_flags = rts_flags(way)

    stats_file = name + '.stats'
    if len(opts.stats_range_fields) > 0:
        stats_args = ' +RTS -V0 -t' + stats_file + ' --machine-readable -RTS'
    else:
        stats_args = ''

    if opts.no_stdin:
        stdin_comes_from = ''
    else:
        stdin_comes_from = ' <' + use_stdin

    if opts.combined_output:
        redirection        = ' > {0} 2>&1'.format(run_stdout)
        redirection_append = ' >> {0} 2>&1'.format(run_stdout)
    else:
        redirection        = ' > {0} 2> {1}'.format(run_stdout, run_stderr)
        redirection_append = ' >> {0} 2>> {1}'.format(run_stdout, run_stderr)

    # Put extra_run_opts last: extra_run_opts('+RTS foo') should work.
    cmd = prog + stats_args + ' '  \
        + my_rts_flags + ' '       \
        + extra_run_opts + ' '     \
        + stdin_comes_from         \
        + redirection

    if opts.cmd_wrapper != None:
        cmd = opts.cmd_wrapper(cmd) + redirection_append

    cmd = 'cd ' + opts.testdir + ' && ' + cmd

    # run the command
    result = runCmdFor(name, cmd, timeout_multiplier=opts.run_timeout_multiplier)

    exit_code = result >> 8
    signal    = result & 0xff

    # check the exit code
    if exit_code != opts.exit_code:
        if config.verbose >= 1 and _expect_pass(way):
            print('Wrong exit code (expected', opts.exit_code, ', actual', exit_code, ')')
            dump_stdout(name)
            dump_stderr(name)
        return failBecause('bad exit code')

    check_hp = my_rts_flags.find("-h") != -1
    check_prof = my_rts_flags.find("-p") != -1

    if not opts.ignore_output:
        bad_stderr = not opts.combined_output and not check_stderr_ok(name, way)
        bad_stdout = not check_stdout_ok(name, way)
        if bad_stderr:
            return failBecause('bad stderr')
        if bad_stdout:
            return failBecause('bad stdout')
        # exit_code > 127 probably indicates a crash, so don't try to run hp2ps.
        if check_hp and (exit_code <= 127 or exit_code == 251) and not check_hp_ok(name):
            return failBecause('bad heap profile')
        if check_prof and not check_prof_ok(name, way):
            return failBecause('bad profile')

    return checkStats(name, way, stats_file, opts.stats_range_fields)

def rts_flags(way):
    if (way == ''):
        return ''
    else:
        args = config.way_rts_flags[way]

    if args == []:
        return ''
    else:
        return '+RTS ' + ' '.join(args) + ' -RTS'

# -----------------------------------------------------------------------------
# Run a program in the interpreter and check its output

def interpreter_run( name, way, extra_hc_opts, compile_only, top_mod ):
    opts = getTestOpts()

    outname = add_suffix(name, 'interp.stdout')
    errname = add_suffix(name, 'interp.stderr')
    rm_no_fail(outname)
    rm_no_fail(errname)
    rm_no_fail(name)

    if (top_mod == ''):
        srcname = add_hs_lhs_suffix(name)
    else:
        srcname = top_mod

    scriptname = add_suffix(name, 'genscript')
    qscriptname = in_testdir(scriptname)
    rm_no_fail(qscriptname)

    delimiter = '===== program output begins here\n'

    script = open(qscriptname, 'w')
    if not compile_only:
        # set the prog name and command-line args to match the compiled
        # environment.
        script.write(':set prog ' + name + '\n')
        script.write(':set args ' + getTestOpts().extra_run_opts + '\n')
        # Add marker lines to the stdout and stderr output files, so we
        # can separate GHCi's output from the program's.
        script.write(':! echo ' + delimiter)
        script.write(':! echo 1>&2 ' + delimiter)
        # Set stdout to be line-buffered to match the compiled environment.
        script.write('System.IO.hSetBuffering System.IO.stdout System.IO.LineBuffering\n')
        # wrapping in GHC.TopHandler.runIO ensures we get the same output
        # in the event of an exception as for the compiled program.
        script.write('GHC.TopHandler.runIOFastExit Main.main Prelude.>> Prelude.return ()\n')
    script.close()

    # figure out what to use for stdin
    if getTestOpts().stdin != '':
        stdin_file = in_testdir(getTestOpts().stdin)
    else:
        stdin_file = qualify(name, 'stdin')

    if os.path.exists(stdin_file):
        os.system('cat ' + stdin_file + ' >>' + qscriptname)

    flags = ' '.join(get_compiler_flags(override_flags=None, noforce=False) +
                     config.way_flags(name)[way])

    if getTestOpts().combined_output:
        redirection        = ' > {0} 2>&1'.format(outname)
        redirection_append = ' >> {0} 2>&1'.format(outname)
    else:
        redirection        = ' > {0} 2> {1}'.format(outname, errname)
        redirection_append = ' >> {0} 2>> {1}'.format(outname, errname)

    cmd = ('{{compiler}} {srcname} {flags} {extra_hc_opts} '
           '< {scriptname} {redirection}'
          ).format(**locals())

    if getTestOpts().cmd_wrapper != None:
        cmd = getTestOpts().cmd_wrapper(cmd) + redirection_append;

    cmd = 'cd ' + getTestOpts().testdir + " && " + cmd

    result = runCmdFor(name, cmd, timeout_multiplier=opts.run_timeout_multiplier)

    exit_code = result >> 8
    signal    = result & 0xff

    # split the stdout into compilation/program output
    split_file(in_testdir(outname), delimiter,
               in_testdir(name, 'comp.stdout'),
               in_testdir(name, 'run.stdout'))
    split_file(in_testdir(errname), delimiter,
               in_testdir(name, 'comp.stderr'),
               in_testdir(name, 'run.stderr'))

    # check the exit code
    if exit_code != getTestOpts().exit_code:
        print('Wrong exit code (expected', getTestOpts().exit_code, ', actual', exit_code, ')')
        dump_stdout(name)
        dump_stderr(name)
        return failBecause('bad exit code')

    # ToDo: if the sub-shell was killed by ^C, then exit

    if getTestOpts().ignore_output or (check_stderr_ok(name, way) and
                                       check_stdout_ok(name, way)):
        return passed()
    else:
        return failBecause('bad stdout or stderr')


def split_file(in_fn, delimiter, out1_fn, out2_fn):
    infile = open(in_fn)
    out1 = open(out1_fn, 'w')
    out2 = open(out2_fn, 'w')

    line = infile.readline()
    line = re.sub('\r', '', line) # ignore Windows EOL
    while (re.sub('^\s*','',line) != delimiter and line != ''):
        out1.write(line)
        line = infile.readline()
        line = re.sub('\r', '', line)
    out1.close()

    line = infile.readline()
    while (line != ''):
        out2.write(line)
        line = infile.readline()
    out2.close()

# -----------------------------------------------------------------------------
# Utils
def get_compiler_flags(override_flags, noforce):
    opts = getTestOpts()

    if override_flags is not None:
        flags = copy.copy(override_flags)
    else:
        flags = copy.copy(opts.compiler_always_flags)

    if noforce:
        flags = [f for f in flags if f != '-fforce-recomp']

    flags.append(opts.extra_hc_opts)

    if opts.outputdir != None:
        flags.extend(["-outputdir", opts.outputdir])

    return flags

def check_stdout_ok(name, way):
   actual_stdout_file = add_suffix(name, 'run.stdout')
   (platform_specific, expected_stdout_file) = find_expected_file(name, 'stdout')

   def norm(str):
      if platform_specific:
         return str
      else:
         return normalise_output(str)

   extra_norm = join_normalisers(norm, getTestOpts().extra_normaliser)

   check_stdout = getTestOpts().check_stdout
   if check_stdout:
      actual_stdout_path = in_testdir(actual_stdout_file)
      return check_stdout(actual_stdout_path, extra_norm)

   return compare_outputs(way, 'stdout', extra_norm,
                          expected_stdout_file, actual_stdout_file)

def dump_stdout( name ):
   print('Stdout:')
   print(read_no_crs(in_testdir(name, 'run.stdout')))

def check_stderr_ok(name, way):
   actual_stderr_file = add_suffix(name, 'run.stderr')
   (platform_specific, expected_stderr_file) = find_expected_file(name, 'stderr')

   def norm(str):
      if platform_specific:
         return str
      else:
         return normalise_errmsg(str)

   return compare_outputs(way, 'stderr',
                          join_normalisers(norm, getTestOpts().extra_errmsg_normaliser), \
                          expected_stderr_file, actual_stderr_file)

def dump_stderr( name ):
   print("Stderr:")
   print(read_no_crs(in_testdir(name, 'run.stderr')))

def read_no_crs(file):
    str = ''
    try:
        h = open(file)
        str = h.read()
        h.close
    except:
        # On Windows, if the program fails very early, it seems the
        # files stdout/stderr are redirected to may not get created
        pass
    return re.sub('\r', '', str)

def write_file(file, str):
    h = open(file, 'w')
    h.write(str)
    h.close

def check_hp_ok(name):

    # do not qualify for hp2ps because we should be in the right directory
    hp2psCmd = "cd " + getTestOpts().testdir + " && {hp2ps} " + name

    hp2psResult = runCmdExitCode(hp2psCmd)

    actual_ps_path = in_testdir(name, 'ps')

    if(hp2psResult == 0):
        if (os.path.exists(actual_ps_path)):
            if gs_working:
                gsResult = runCmdExitCode(genGSCmd(actual_ps_path))
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
    actual_prof_file = add_suffix(name, 'prof')
    actual_prof_path = in_testdir(actual_prof_file)

    if not os.path.exists(actual_prof_path):
        print(actual_prof_path + " does not exist")
        return(False)

    if os.path.getsize(actual_prof_path) == 0:
        print(actual_prof_path + " is empty")
        return(False)

    (_, expected_prof_file) = find_expected_file(name, 'prof.sample')
    expected_prof_path = in_testdir(expected_prof_file)

    # sample prof file is not required
    if not os.path.exists(expected_prof_path):
        return True
    else:
        return compare_outputs(way, 'prof', normalise_prof,
                               expected_prof_file, actual_prof_file,
                               whitespace_normaliser=normalise_whitespace)

# Compare expected output to actual output, and optionally accept the
# new output. Returns true if output matched or was accepted, false
# otherwise. See Note [Output comparison] for the meaning of the
# normaliser and whitespace_normaliser parameters.
def compare_outputs(way, kind, normaliser, expected_file, actual_file,
                    whitespace_normaliser=lambda x:x):

    expected_path = in_testdir(expected_file)
    actual_path = in_testdir(actual_file)

    if os.path.exists(expected_path):
        expected_str = normaliser(read_no_crs(expected_path))
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
            r = os.system('diff -uw {} {}'.format(expected_normalised_path,
                                                  actual_normalised_path))

            # If for some reason there were no non-whitespace differences,
            # then do a full diff
            if r == 0:
                r = os.system('diff -u {} {}'.format(expected_normalised_path,
                                                     actual_normalised_path))

        if config.accept and (getTestOpts().expect == 'fail' or
                              way in getTestOpts().expect_fail_for):
            if_verbose(1, 'Test is expected to fail. Not accepting new output.')
            return 0
        elif config.accept:
            if_verbose(1, 'Accepting new output.')
            write_file(expected_path, actual_raw)
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
    str = re.sub('[ \t\n]+', ' ', str)
    return str.strip()

def normalise_callstacks(str):
    def repl(matches):
        location = matches.group(1)
        location = normalise_slashes_(location)
        return ', called at {}:<line>:<column> in'.format(location)
    # Ignore line number differences in call stacks (#10834).
    return re.sub(', called at (.+):[\\d]+:[\\d]+ in', repl, str)

def normalise_errmsg( str ):
    # remove " error:" and lower-case " Warning:" to make patch for
    # trac issue #10021 smaller
    str = modify_lines(str, lambda l: re.sub(' error:', '', l))
    str = modify_lines(str, lambda l: re.sub(' Warning:', ' warning:', l))
    str = normalise_callstacks(str)

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

    # We have somthing like this:

    # MAIN      MAIN                 101      0    0.0    0.0   100.0  100.0
    # k         Main                 204      1    0.0    0.0     0.0    0.0
    #  foo      Main                 205      1    0.0    0.0     0.0    0.0
    #   foo.bar Main                 207      1    0.0    0.0     0.0    0.0

    # then we remove all the specific profiling data, leaving only the
    # cost centre name, module, and entries, to end up with this:

    # MAIN                MAIN            0
    #   k                 Main            1
    #    foo              Main            1
    #     foo.bar         Main            1

    str = re.sub('\n([ \t]*[^ \t]+)([ \t]+[^ \t]+)([ \t]+\\d+)([ \t]+\\d+)[ \t]+([\\d\\.]+)[ \t]+([\\d\\.]+)[ \t]+([\\d\\.]+)[ \t]+([\\d\\.]+)','\n\\1 \\2 \\4',str)
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
            print(open(f).read())
        except:
            print('')

def rawSystem(cmd_and_args):
    # We prefer subprocess.call to os.spawnv as the latter
    # seems to send its arguments through a shell or something
    # with the Windows (non-cygwin) python. An argument "a b c"
    # turns into three arguments ["a", "b", "c"].

    cmd = cmd_and_args[0]
    return subprocess.call([strip_quotes(cmd)] + cmd_and_args[1:])

# Note that this doesn't handle the timeout itself; it is just used for
# commands that have timeout handling built-in.
def rawSystemWithTimeout(cmd_and_args):
    r = rawSystem(cmd_and_args)
    if r == 98:
        # The python timeout program uses 98 to signal that ^C was pressed
        stopNow()
    if r == 99 and getTestOpts().exit_code != 99:
        # Only print a message when timeout killed the process unexpectedly.
        cmd = cmd_and_args[-1]
        if_verbose(1, 'Timeout happened...killed process "{}"...\n'.format(cmd))
    return r

# cmd is a complex command in Bourne-shell syntax
# e.g (cd . && 'c:/users/simonpj/darcs/HEAD/compiler/stage1/ghc-inplace' ...etc)
# Hence it must ultimately be run by a Bourne shell
#
# Mostly it invokes the command wrapped in 'timeout' thus
#  timeout 300 'cd . && ...blah blah'
# so it's timeout's job to invoke the Bourne shell
#
# But watch out for the case when there is no timeout program!
# Then, when using the native Python, os.system will invoke the cmd shell

def runCmd( cmd ):
    # Format cmd using config. Example: cmd='{hpc} report A.tix'
    cmd = cmd.format(**config.__dict__)

    if_verbose( 3, cmd )
    r = 0
    if config.os == 'mingw32':
        # On MinGW, we will always have timeout
        assert config.timeout_prog!=''

    if config.timeout_prog != '':
        r = rawSystemWithTimeout([config.timeout_prog, str(config.timeout), cmd])
    else:
        r = os.system(cmd)
    return r << 8

def runCmdFor( name, cmd, timeout_multiplier=1.0 ):
    # Format cmd using config. Example: cmd='{hpc} report A.tix'
    cmd = cmd.format(**config.__dict__)

    if_verbose( 3, cmd )
    r = 0
    if config.os == 'mingw32':
        # On MinGW, we will always have timeout
        assert config.timeout_prog!=''
    timeout = int(ceil(config.timeout * timeout_multiplier))

    if config.timeout_prog != '':
        if config.check_files_written:
            fn = name + ".strace"
            r = rawSystemWithTimeout(
                    ["strace", "-o", fn, "-fF",
                               "-e", "creat,open,chdir,clone,vfork",
                     strip_quotes(config.timeout_prog), str(timeout), cmd])
            addTestFilesWritten(name, fn)
            rm_no_fail(fn)
        else:
            r = rawSystemWithTimeout([config.timeout_prog, str(timeout), cmd])
    else:
        r = os.system(cmd)
    return r << 8

def runCmdExitCode( cmd ):
    return (runCmd(cmd) >> 8);


# -----------------------------------------------------------------------------
# checking for files being written to by multiple tests

re_strace_call_end = '(\) += ([0-9]+|-1 E.*)| <unfinished ...>)$'
re_strace_unavailable_end ='\) += \? <unavailable>$'

re_strace_unavailable_line  = re.compile('^' + re_strace_unavailable_end)
re_strace_unavailable_cntnt = re.compile('^<\.\.\. .* resumed> ' + re_strace_unavailable_end)
re_strace_pid               = re.compile('^([0-9]+) +(.*)')
re_strace_clone             = re.compile('^(clone\(|<... clone resumed> ).*\) = ([0-9]+)$')
re_strace_clone_unfinished  = re.compile('^clone\( <unfinished \.\.\.>$')
re_strace_vfork             = re.compile('^(vfork\(\)|<\.\.\. vfork resumed> \)) += ([0-9]+)$')
re_strace_vfork_unfinished  = re.compile('^vfork\( <unfinished \.\.\.>$')
re_strace_chdir             = re.compile('^chdir\("([^"]*)"(\) += 0| <unfinished ...>)$')
re_strace_chdir_resumed     = re.compile('^<\.\.\. chdir resumed> \) += 0$')
re_strace_open              = re.compile('^open\("([^"]*)", ([A-Z_|]*)(, [0-9]+)?' + re_strace_call_end)
re_strace_open_resumed      = re.compile('^<... open resumed> '                    + re_strace_call_end)
re_strace_ignore_sigchild   = re.compile('^--- SIGCHLD \(Child exited\) @ 0 \(0\) ---$')
re_strace_ignore_sigchild2  = re.compile('^--- SIGCHLD {si_signo=SIGCHLD, si_code=CLD_EXITED, .*} ---$')
re_strace_ignore_exited     = re.compile('^\+\+\+ exited with [0-9]* \+\+\+$')
re_strace_ignore_sigvtalarm = re.compile('^--- SIGVTALRM \(Virtual timer expired\) @ 0 \(0\) ---$')
re_strace_ignore_sigvtalarm2= re.compile('^--- SIGVTALRM {si_signo=SIGVTALRM, si_code=SI_TIMER, .*} ---$')
re_strace_ignore_sigint     = re.compile('^--- SIGINT \(Interrupt\) @ 0 \(0\) ---$')
re_strace_ignore_sigfpe     = re.compile('^--- SIGFPE \(Floating point exception\) @ 0 \(0\) ---$')
re_strace_ignore_sigsegv    = re.compile('^--- SIGSEGV \(Segmentation fault\) @ 0 \(0\) ---$')
re_strace_ignore_sigpipe    = re.compile('^--- SIGPIPE \(Broken pipe\) @ 0 \(0\) ---$')

# Files that are read or written but shouldn't be:
# * ghci_history shouldn't be read or written by tests
# * things under package.conf.d shouldn't be written by tests
bad_file_usages = {}

# Mapping from tests to the list of files that they write
files_written = {}

# Mapping from tests to the list of files that they write but don't clean
files_written_not_removed = {}

def add_bad_file_usage(name, file):
    try:
        if not file in bad_file_usages[name]:
            bad_file_usages[name].append(file)
    except:
        bad_file_usages[name] = [file]

def mkPath(curdir, path):
    # Given the current full directory is 'curdir', what is the full
    # path to 'path'?
    return os.path.realpath(os.path.join(curdir, path))

def addTestFilesWritten(name, fn):
    if config.use_threads:
        with t.lockFilesWritten:
            addTestFilesWrittenHelper(name, fn)
    else:
        addTestFilesWrittenHelper(name, fn)

def addTestFilesWrittenHelper(name, fn):
    started = False
    working_directories = {}

    with open(fn, 'r') as f:
        for line in f:
            m_pid = re_strace_pid.match(line)
            if m_pid:
                pid = m_pid.group(1)
                content = m_pid.group(2)
            elif re_strace_unavailable_line.match(line):
                next
            else:
                framework_fail(name, 'strace', "Can't find pid in strace line: " + line)

            m_open = re_strace_open.match(content)
            m_chdir = re_strace_chdir.match(content)
            m_clone = re_strace_clone.match(content)
            m_vfork = re_strace_vfork.match(content)

            if not started:
                working_directories[pid] = os.getcwd()
                started = True

            if m_open:
                file = m_open.group(1)
                file = mkPath(working_directories[pid], file)
                if file.endswith("ghci_history"):
                    add_bad_file_usage(name, file)
                elif not file in ['/dev/tty', '/dev/null'] and not file.startswith("/tmp/ghc"):
                    flags = m_open.group(2).split('|')
                    if 'O_WRONLY' in flags or 'O_RDWR' in flags:
                        if re.match('package\.conf\.d', file):
                            add_bad_file_usage(name, file)
                        else:
                            try:
                                if not file in files_written[name]:
                                    files_written[name].append(file)
                            except:
                                files_written[name] = [file]
                    elif 'O_RDONLY' in flags:
                        pass
                    else:
                        framework_fail(name, 'strace', "Can't understand flags in open strace line: " + line)
            elif m_chdir:
                # We optimistically assume that unfinished chdir's are going to succeed
                dir = m_chdir.group(1)
                working_directories[pid] = mkPath(working_directories[pid], dir)
            elif m_clone:
                working_directories[m_clone.group(2)] = working_directories[pid]
            elif m_vfork:
                working_directories[m_vfork.group(2)] = working_directories[pid]
            elif re_strace_open_resumed.match(content):
                pass
            elif re_strace_chdir_resumed.match(content):
                pass
            elif re_strace_vfork_unfinished.match(content):
                pass
            elif re_strace_clone_unfinished.match(content):
                pass
            elif re_strace_ignore_sigchild.match(content):
                pass
            elif re_strace_ignore_sigchild2.match(content):
                pass
            elif re_strace_ignore_exited.match(content):
                pass
            elif re_strace_ignore_sigvtalarm.match(content):
                pass
            elif re_strace_ignore_sigvtalarm2.match(content):
                pass
            elif re_strace_ignore_sigint.match(content):
                pass
            elif re_strace_ignore_sigfpe.match(content):
                pass
            elif re_strace_ignore_sigsegv.match(content):
                pass
            elif re_strace_ignore_sigpipe.match(content):
                pass
            elif re_strace_unavailable_cntnt.match(content):
                pass
            else:
                framework_fail(name, 'strace', "Can't understand strace line: " + line)
 
def checkForFilesWrittenProblems(file):
    foundProblem = False

    files_written_inverted = {}
    for t in files_written.keys():
        for f in files_written[t]:
            try:
                files_written_inverted[f].append(t)
            except:
                files_written_inverted[f] = [t]

    for f in files_written_inverted.keys():
        if len(files_written_inverted[f]) > 1:
            if not foundProblem:
                foundProblem = True
                file.write("\n")
                file.write("\nSome files are written by multiple tests:\n")
            file.write("    " + f + " (" + str(files_written_inverted[f]) + ")\n")
    if foundProblem:
        file.write("\n")

    # -----

    if len(files_written_not_removed) > 0:
        file.write("\n")
        file.write("\nSome files written but not removed:\n")
        tests = list(files_written_not_removed.keys())
        tests.sort()
        for t in tests:
            for f in files_written_not_removed[t]:
                file.write("    " + t + ": " + f + "\n")
        file.write("\n")

    # -----

    if len(bad_file_usages) > 0:
        file.write("\n")
        file.write("\nSome bad file usages:\n")
        tests = list(bad_file_usages.keys())
        tests.sort()
        for t in tests:
            for f in bad_file_usages[t]:
                file.write("    " + t + ": " + f + "\n")
        file.write("\n")

# -----------------------------------------------------------------------------
# checking if ghostscript is available for checking the output of hp2ps

def genGSCmd(psfile):
    return (config.gs + ' -dNODISPLAY -dBATCH -dQUIET -dNOPAUSE ' + psfile);

def gsNotWorking():
    global gs_working
    print("GhostScript not available for hp2ps tests")

global gs_working
gs_working = 0
if config.have_profiling:
  if config.gs != '':
    resultGood = runCmdExitCode(genGSCmd(config.confdir + '/good.ps'));
    if resultGood == 0:
        resultBad = runCmdExitCode(genGSCmd(config.confdir + '/bad.ps') +
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

def rm_no_fail( file ):
   try:
       os.remove( file )
   finally:
       return

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
    return getTestOpts().testdir + '/' + add_suffix(name, suffix)

def qualify( name, suff ):
    return in_testdir(add_suffix(name, suff))


# Finding the sample output.  The filename is of the form
#
#   <test>.stdout[-ws-<wordsize>][-<platform>]
#
# and we pick the most specific version available.  The <version> is
# the major version of the compiler (e.g. 6.8.2 would be "6.8").  For
# more fine-grained control use compiler_lt().
#
def find_expected_file(name, suff):
    basename = add_suffix(name, suff)
    basepath = in_testdir(basename)

    files = [(platformSpecific, basename + ws + plat)
             for (platformSpecific, plat) in [(1, '-' + config.platform),
                                              (1, '-' + config.os),
                                              (0, '')]
             for ws in ['-ws-' + config.wordsize, '']]

    dir = glob.glob(basepath + '*')
    dir = [normalise_slashes_(d) for d in dir]

    for (platformSpecific, f) in files:
       if in_testdir(f) in dir:
            return (platformSpecific,f)

    return (0, basename)

# Clean up prior to the test, so that we can't spuriously conclude
# that it passed on the basis of old run outputs.
def pretest_cleanup(name):
   if getTestOpts().outputdir != None:
       odir = in_testdir(getTestOpts().outputdir)
       try:
           shutil.rmtree(odir)
       except:
           pass
       os.mkdir(odir)

   rm_no_fail(qualify(name,'interp.stderr'))
   rm_no_fail(qualify(name,'interp.stdout'))
   rm_no_fail(qualify(name,'comp.stderr'))
   rm_no_fail(qualify(name,'comp.stdout'))
   rm_no_fail(qualify(name,'run.stderr'))
   rm_no_fail(qualify(name,'run.stdout'))
   rm_no_fail(qualify(name,'tix'))
   rm_no_fail(qualify(name,'exe.tix'))
   # simple_build zaps the following:
   # rm_nofail(qualify("o"))
   # rm_nofail(qualify(""))
   # not interested in the return code

# -----------------------------------------------------------------------------
# Return a list of all the files ending in '.T' below directories roots.

def findTFiles(roots):
    # It would be better to use os.walk, but that
    # gives backslashes on Windows, which trip the
    # testsuite later :-(
    return [filename for root in roots for filename in findTFiles_(root)]

def findTFiles_(path):
    if os.path.isdir(path):
        paths = [path + '/' + x for x in os.listdir(path)]
        return findTFiles(paths)
    elif path[-2:] == '.T':
        return [path]
    else:
        return []

# -----------------------------------------------------------------------------
# Output a test summary to the specified file object

def summary(t, file, short=False):

    file.write('\n')
    printUnexpectedTests(file, [t.unexpected_passes, t.unexpected_failures, t.unexpected_stat_failures])

    if short:
        # Only print the list of unexpected tests above.
        return

    file.write('OVERALL SUMMARY for test run started at '
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
               + repr(t.n_missing_libs).rjust(8)
               + ' had missing libraries\n'
               + repr(t.n_expected_passes).rjust(8)
               + ' expected passes\n'
               + repr(t.n_expected_failures).rjust(8)
               + ' expected failures\n'
               + '\n'
               + repr(t.n_framework_failures).rjust(8)
               + ' caused framework failures\n'
               + repr(t.n_unexpected_passes).rjust(8)
               + ' unexpected passes\n'
               + repr(t.n_unexpected_failures).rjust(8)
               + ' unexpected failures\n'
               + repr(t.n_unexpected_stat_failures).rjust(8)
               + ' unexpected stat failures\n'
               + '\n')

    if t.n_unexpected_passes > 0:
        file.write('Unexpected passes:\n')
        printPassingTestInfosSummary(file, t.unexpected_passes)

    if t.n_unexpected_failures > 0:
        file.write('Unexpected failures:\n')
        printFailingTestInfosSummary(file, t.unexpected_failures)

    if t.n_unexpected_stat_failures > 0:
        file.write('Unexpected stat failures:\n')
        printFailingTestInfosSummary(file, t.unexpected_stat_failures)

    if config.check_files_written:
        checkForFilesWrittenProblems(file)

    if stopping():
        file.write('WARNING: Testsuite run was terminated early\n')

def printUnexpectedTests(file, testInfoss):
    unexpected = []
    for testInfos in testInfoss:
        directories = testInfos.keys()
        for directory in directories:
            tests = list(testInfos[directory].keys())
            unexpected += tests
    if unexpected != []:
        file.write('Unexpected results from:\n')
        file.write('TEST="' + ' '.join(unexpected) + '"\n')
        file.write('\n')

def printPassingTestInfosSummary(file, testInfos):
    directories = list(testInfos.keys())
    directories.sort()
    maxDirLen = max(len(x) for x in directories)
    for directory in directories:
        tests = list(testInfos[directory].keys())
        tests.sort()
        for test in tests:
           file.write('   ' + directory.ljust(maxDirLen + 2) + test + \
                      ' (' + ','.join(testInfos[directory][test]) + ')\n')
    file.write('\n')

def printFailingTestInfosSummary(file, testInfos):
    directories = list(testInfos.keys())
    directories.sort()
    maxDirLen = max(len(d) for d in directories)
    for directory in directories:
        tests = list(testInfos[directory].keys())
        tests.sort()
        for test in tests:
           reasons = testInfos[directory][test].keys()
           for reason in reasons:
               file.write('   ' + directory.ljust(maxDirLen + 2) + test + \
                          ' [' + reason + ']' + \
                          ' (' + ','.join(testInfos[directory][test][reason]) + ')\n')
    file.write('\n')

def modify_lines(s, f):
    return '\n'.join([f(l) for l in s.splitlines()])
