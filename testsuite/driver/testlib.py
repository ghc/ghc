#
# (c) Simon Marlow 2002
#

# This allows us to use the "with X:" syntax with python 2.5:
from __future__ import with_statement

import shutil
import sys
import os
import errno
import string
import re
import traceback
import copy
import glob
import types
import math

have_subprocess = False
try:
    import subprocess
    have_subprocess = True
except:
    print "Warning: subprocess not found, will fall back to spawnv"

from string import join
from testglobals import *
from testutil import *

if config.use_threads:
    import threading
    import thread

# Options valid for all the tests in the current "directory".  After
# each test, we reset the options to these.  To change the options for
# multiple tests, the function setTestOpts() below can be used to alter
# these options.
global thisdir_testopts
thisdir_testopts = TestOptions()

def getThisDirTestOpts():
    return thisdir_testopts

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

# This can be called at the top of a file of tests, to set default test options
# for the following tests.
def setTestOpts( f ):
    f( thisdir_testopts );

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

def normal( opts ):
    return;

def skip( opts ):
    opts.skip = 1

def expect_fail( opts ):
    opts.expect = 'fail';

def reqlib( lib ):
    return lambda opts, l=lib: _reqlib (opts, l )

# Cache the results of looking to see if we have a library or not.
# This makes quite a difference, especially on Windows.
have_lib = {}

def _reqlib( opts, lib ):
    if have_lib.has_key(lib):
        got_it = have_lib[lib]
    else:
        if have_subprocess:
            # By preference we use subprocess, as the alternative uses
            # /dev/null which mingw doesn't have.
            p = subprocess.Popen([config.ghc_pkg, '--no-user-package-db', 'describe', lib],
                                 stdout=subprocess.PIPE,
                                 stderr=subprocess.PIPE)
            # read from stdout and stderr to avoid blocking due to
            # buffers filling
            p.communicate()
            r = p.wait()
        else:
            r = os.system(config.ghc_pkg + ' describe ' + lib
                                         + ' > /dev/null 2> /dev/null')
        got_it = r == 0
        have_lib[lib] = got_it

    if not got_it:
        opts.expect = 'missing-lib'

def req_profiling( opts ):
    if not config.have_profiling:
        opts.expect = 'fail'

def req_shared_libs( opts ):
    if not config.have_shared_libs:
        opts.expect = 'fail'

def req_interp( opts ):
    if not config.have_interp:
        opts.expect = 'fail'

def req_smp( opts ):
    if not config.have_smp:
        opts.expect = 'fail'

def expect_broken( bug ):
    return lambda opts, b=bug: _expect_broken (opts, b )

def _expect_broken( opts, bug ):
    opts.expect = 'fail';

def ignore_output( opts ):
    opts.ignore_output = 1

def no_stdin( opts ):
    opts.no_stdin = 1

def combined_output( opts ):
    opts.combined_output = True

# -----

def expect_fail_for( ways ):
    return lambda opts, w=ways: _expect_fail_for( opts, w )

def _expect_fail_for( opts, ways ):
    opts.expect_fail_for = ways

def expect_broken_for( bug, ways ):
    return lambda opts, b=bug, w=ways: _expect_broken_for( opts, b, w )

def _expect_broken_for( opts, bug, ways ):
    opts.expect_fail_for = ways

# -----

def omit_ways( ways ):
    return lambda opts, w=ways: _omit_ways( opts, w )

def _omit_ways( opts, ways ):
    opts.omit_ways = ways

# -----

def only_ways( ways ):
    return lambda opts, w=ways: _only_ways( opts, w )

def _only_ways( opts, ways ):
    opts.only_ways = ways

# -----

def extra_ways( ways ):
    return lambda opts, w=ways: _extra_ways( opts, w )

def _extra_ways( opts, ways ):
    opts.extra_ways = ways

# -----

def omit_compiler_types( compiler_types ):
   return lambda opts, c=compiler_types: _omit_compiler_types(opts, c)

def _omit_compiler_types( opts, compiler_types ):
    if config.compiler_type in compiler_types:
        opts.skip = 1

# -----

def only_compiler_types( compiler_types ):
   return lambda opts, c=compiler_types: _only_compiler_types(opts, c)

def _only_compiler_types( opts, compiler_types ):
    if config.compiler_type not in compiler_types:
        opts.skip = 1

# -----

def set_stdin( file ):
   return lambda opts, f=file: _set_stdin(opts, f);

def _set_stdin( opts, f ):
   opts.stdin = f

# -----

def exit_code( val ):
    return lambda opts, v=val: _exit_code(opts, v);

def _exit_code( opts, v ):
    opts.exit_code = v

# -----

def timeout_multiplier( val ):
    return lambda opts, v=val: _timeout_multiplier(opts, v)

def _timeout_multiplier( opts, v ):
    opts.timeout_multiplier = v

# -----

def extra_run_opts( val ):
    return lambda opts, v=val: _extra_run_opts(opts, v);

def _extra_run_opts( opts, v ):
    opts.extra_run_opts = v

# -----

def extra_hc_opts( val ):
    return lambda opts, v=val: _extra_hc_opts(opts, v);

def _extra_hc_opts( opts, v ):
    opts.extra_hc_opts = v

# -----

def extra_clean( files ):
    return lambda opts, v=files: _extra_clean(opts, v);

def _extra_clean( opts, v ):
    opts.clean_files = v

# -----

def stats_num_field( field, min, max ):
    return lambda opts, f=field, x=min, y=max: _stats_num_field(opts, f, x, y);

def _stats_num_field( opts, f, x, y ):
    # copy the dictionary, as the config gets shared between all tests
    opts.stats_num_fields = opts.stats_num_fields.copy()
    opts.stats_num_fields[f] = (x, y)

def compiler_stats_num_field( field, min, max ):
    return lambda opts, f=field, x=min, y=max: _compiler_stats_num_field(opts, f, x, y);

def _compiler_stats_num_field( opts, f, x, y ):
    # copy the dictionary, as the config gets shared between all tests
    opts.compiler_stats_num_fields = opts.compiler_stats_num_fields.copy()
    opts.compiler_stats_num_fields[f] = (x, y)

# -----

def stats_range_field( field, min, max ):
    return lambda opts, f=field, x=min, y=max: _stats_range_field(opts, f, x, y);

def _stats_range_field( opts, f, x, y ):
    # copy the dictionary, as the config gets shared between all tests
    opts.stats_range_fields = opts.stats_range_fields.copy()
    opts.stats_range_fields[f] = (x, y)

def compiler_stats_range_field( field, min, max ):
    return lambda opts, f=field, x=min, y=max: _compiler_stats_range_field(opts, f, x, y);

def _compiler_stats_range_field( opts, f, x, y ):
    # copy the dictionary, as the config gets shared between all tests
    opts.compiler_stats_range_fields = opts.compiler_stats_range_fields.copy()
    opts.compiler_stats_range_fields[f] = (x, y)

# -----

def skip_if_no_ghci(opts):
    if not ('ghci' in config.run_ways):
        opts.skip = 1

# ----

def skip_if_fast(opts):
    if config.fast:
        opts.skip = 1

# -----

def if_platform( plat, f ):
    if config.platform == plat:
        return f
    else:
        return normal

def unless_platform( plat, f ):
    if config.platform != plat:
        return f
    else:
        return normal

def if_os( os, f ):
    if config.os == os:
        return f
    else:
        return normal

def unless_os( os, f ):
    if config.os == os:
        return normal
    else:
        return f

def if_arch( arch, f ):
    if config.arch == arch:
        return f
    else:
        return normal

def unless_arch( arch, f ):
    if config.arch == arch:
        return normal
    else:
        return f

def if_wordsize( ws, f ):
    if config.wordsize == str(ws):
        return f
    else:
        return normal

def unless_wordsize( ws, f ):
    if config.wordsize == str(ws):
        return normal
    else:
        return f

def if_unregisterised( f ):
    if config.unregisterised:
        return f
    else:
        return normal

def unless_unregisterised( f ):
    if config.unregisterised:
        return normal
    else:
        return f

def if_msys( f ):
    if config.msys:
        return f
    else:
        return normal

def if_cygwin( f ):
    if config.cygwin:
        return f
    else:
        return normal

def when_have_vanilla( f ):
    if config.have_vanilla:
        return f
    else:
        return normal

def unless_have_vanilla( f ):
    if config.have_vanilla:
        return normal
    else:
        return f

def when_have_dynamic( f ):
    if config.have_dynamic:
        return f
    else:
        return normal

def unless_have_dynamic( f ):
    if config.have_dynamic:
        return normal
    else:
        return f

def when_have_profiling( f ):
    if config.have_profiling:
        return f
    else:
        return normal

def unless_have_profiling( f ):
    if config.have_profiling:
        return normal
    else:
        return f

# ---

def if_ghci_dynamic( f ):
    if config.ghc_dynamic_by_default:
        return f
    else:
        return normal

def if_in_tree_compiler( f ):
    if config.in_tree_compiler:
        return f
    else:
        return normal

def unless_in_tree_compiler( f ):
    if config.in_tree_compiler:
        return normal
    else:
        return f

def if_compiler_type( compiler, f ):
    if config.compiler_type == compiler:
        return f
    else:
        return normal

def if_compiler_profiled( f ):
    if config.compiler_profiled:
        return f
    else:
        return normal

def unless_compiler_profiled( f ):
    if config.compiler_profiled:
        return normal
    else:
        return f

def if_compiler_lt( compiler, version, f ):
    if config.compiler_type == compiler and \
       version_lt(config.compiler_version, version):
        return f
    else:
        return normal

def if_compiler_le( compiler, version, f ):
    if config.compiler_type == compiler and \
       version_le(config.compiler_version, version):
        return f
    else:
        return normal

def if_compiler_gt( compiler, version, f ):
    if config.compiler_type == compiler and \
       version_gt(config.compiler_version, version):
        return f
    else:
        return normal

def if_compiler_ge( compiler, version, f ):
    if config.compiler_type == compiler and \
       version_ge(config.compiler_version, version):
        return f
    else:
        return normal

def if_compiler_debugged( f ):
    if config.compiler_debugged:
        return f
    else:
        return normal

def namebase( nb ):
   return lambda opts, nb=nb: _namebase(opts, nb)

def _namebase( opts, nb ):
    opts.with_namebase = nb

# ---

def if_tag( tag, f ):
    if tag in config.compiler_tags:
        return f
    else:
        return normal

def unless_tag( tag, f ):
    if not (tag in config.compiler_tags):
        return f
    else:
        return normal

# ---
def high_memory_usage(opts):
    opts.alone = True

# ---
def literate( opts ):
    opts.literate = 1;

def c_src( opts ):
    opts.c_src = 1;

def objc_src( opts ):
    opts.objc_src = 1;

def objcpp_src( opts ):
    opts.objcpp_src = 1;

def cmm_src( opts ):
    opts.cmm_src = 1;

def outputdir( odir ):
    return lambda opts, d=odir: _outputdir(opts, d)

def _outputdir( opts, odir ):
    opts.outputdir = odir;

# ----

def pre_cmd( cmd ):
    return lambda opts, c=cmd: _pre_cmd(opts, cmd)

def _pre_cmd( opts, cmd ):
    opts.pre_cmd = cmd

# ----

def clean_cmd( cmd ):
    return lambda opts, c=cmd: _clean_cmd(opts, cmd)

def _clean_cmd( opts, cmd ):
    opts.clean_cmd = cmd

# ----

def cmd_prefix( prefix ):
    return lambda opts, p=prefix: _cmd_prefix(opts, prefix)

def _cmd_prefix( opts, prefix ):
    opts.cmd_wrapper = lambda cmd, p=prefix: p + ' ' + cmd;

# ----

def cmd_wrapper( fun ):
    return lambda opts, f=fun: _cmd_wrapper(opts, fun)

def _cmd_wrapper( opts, fun ):
    opts.cmd_wrapper = fun

# ----

def compile_cmd_prefix( prefix ):
    return lambda opts, p=prefix: _compile_cmd_prefix(opts, prefix)

def _compile_cmd_prefix( opts, prefix ):
    opts.compile_cmd_prefix = prefix

# ----

def normalise_slashes( opts ):
    opts.extra_normaliser = normalise_slashes_

def normalise_exe( opts ):
    opts.extra_normaliser = normalise_exe_

def normalise_fun( fun ):
    return lambda opts, f=fun: _normalise_fun(opts, f)

def _normalise_fun( opts, f ):
    opts.extra_normaliser = f

def normalise_errmsg_fun( fun ):
    return lambda opts, f=fun: _normalise_errmsg_fun(opts, f)

def _normalise_errmsg_fun( opts, f ):
    opts.extra_errmsg_normaliser = f

def two_normalisers(f, g):
    return lambda x, f=f, g=g: f(g(x))

# ----
# Function for composing two opt-fns together

def composes( fs ):
    return reduce(lambda f, g: compose(f, g), fs)

def compose( f, g ):
    return lambda opts, f=f, g=g: _compose(opts,f,g)

def _compose( opts, f, g ):
    f(opts)
    g(opts)

# -----------------------------------------------------------------------------
# The current directory of tests

def newTestDir( dir ):
    global thisdir_testopts
    # reset the options for this test directory
    thisdir_testopts = copy.copy(default_testopts)
    thisdir_testopts.testdir = dir
    thisdir_testopts.compiler_always_flags = config.compiler_always_flags

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
    global aloneTests
    global parallelTests
    global allTestNames
    if name in allTestNames:
        framework_fail(name, 'duplicate', 'There are multiple tests with this name')
    myTestOpts = copy.copy(thisdir_testopts)

    if type(setup) is types.ListType:
       setup = composes(setup)

    setup(myTestOpts)

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
        all_ways = all_ways + filter(lambda way: way not in all_ways,
                                     opts.extra_ways)

        t.total_test_cases = t.total_test_cases + len(all_ways)

        ok_way = lambda way: \
            not getTestOpts().skip \
            and (config.only == [] or name in config.only) \
            and (getTestOpts().only_ways == None or way in getTestOpts().only_ways) \
            and (config.cmdline_ways == [] or way in config.cmdline_ways) \
            and way not in getTestOpts().omit_ways

        # Which ways we are asked to skip
        do_ways = filter (ok_way,all_ways)

        # In fast mode, we skip all but one way
        if config.fast and len(do_ways) > 0:
            do_ways = [do_ways[0]]

        if not config.clean_only:
            # Run the required tests...
            for way in do_ways:
                do_test (name, way, func, args)

            for way in all_ways:
                if way not in do_ways:
                    skiptest (name,way)

        if getTestOpts().cleanup != '' and (config.clean_only or do_ways != []):
            clean(map (lambda suff: name + suff,
                      ['', '.exe', '.exe.manifest', '.genscript',
                       '.stderr.normalised',        '.stdout.normalised',
                       '.run.stderr',               '.run.stdout',
                       '.run.stderr.normalised',    '.run.stdout.normalised',
                       '.comp.stderr',              '.comp.stdout',
                       '.comp.stderr.normalised',   '.comp.stdout.normalised',
                       '.interp.stderr',            '.interp.stdout',
                       '.interp.stderr.normalised', '.interp.stdout.normalised',
                       '.stats', '.comp.stats',
                       '.hi', '.o', '.prof', '.exe.prof', '.hc',
                       '_stub.h', '_stub.c', '_stub.o',
                       '.hp', '.exe.hp', '.ps', '.aux', '.hcr', '.eventlog']))

            if func == multi_compile or func == multi_compile_fail:
                    extra_mods = args[1]
                    clean(map (lambda (f,x): replace_suffix(f, 'o'), extra_mods))
                    clean(map (lambda (f,x): replace_suffix(f, 'hi'), extra_mods))

            clean(getTestOpts().clean_files)

            if getTestOpts().outputdir != None:
                odir = in_testdir(getTestOpts().outputdir)
                try:
                    shutil.rmtree(odir)
                except:
                    pass

            try:
                cleanCmd = getTestOpts().clean_cmd
                if cleanCmd != None:
                    result = runCmdFor(name, 'cd ' + getTestOpts().testdir + ' && ' + cleanCmd)
                    if result != 0:
                        framework_fail(name, 'cleaning', 'clean-command failed: ' + str(result))
            except e:
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
    except Exception, e:
        framework_fail(name, 'runTest', 'Unhandled exception: ' + str(e))

def clean(strs):
    for str in strs:
        for name in glob.glob(in_testdir(str)):
            clean_full_path(name)

def clean_full_path(name):
        try:
            # Remove files...
            os.remove(name)
        except OSError, e1:
            try:
                # ... and empty directories
                os.rmdir(name)
            except OSError, e2:
                # We don't want to fail here, but we do want to know
                # what went wrong, so print out the exceptions.
                # ENOENT isn't a problem, though, as we clean files
                # that don't necessarily exist.
                if e1.errno != errno.ENOENT:
                    print e1
                if e2.errno != errno.ENOENT:
                    print e2

def do_test(name, way, func, args):
    full_name = name + '(' + way + ')'

    try:
        print '=====>', full_name, t.total_tests, 'of', len(allTestNames), \
                        str([t.n_unexpected_passes,   \
                             t.n_unexpected_failures, \
                             t.n_framework_failures])

        if config.use_threads:
            t.lock.release()

        try:
            preCmd = getTestOpts().pre_cmd
            if preCmd != None:
                result = runCmdFor(name, 'cd ' + getTestOpts().testdir + ' && ' + preCmd)
                if result != 0:
                    framework_fail(name, way, 'pre-command failed: ' + str(result))
        except e:
            framework_fail(name, way, 'pre-command exception')

        try:
            result = apply(func, [name,way] + args)
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
            if getTestOpts().expect == 'pass' \
               and way not in getTestOpts().expect_fail_for:
                t.n_expected_passes = t.n_expected_passes + 1
                if name in t.expected_passes:
                    t.expected_passes[name].append(way)
                else:
                    t.expected_passes[name] = [way]
            else:
                print '*** unexpected pass for', full_name
                t.n_unexpected_passes = t.n_unexpected_passes + 1
                addPassingTestInfo(t.unexpected_passes, getTestOpts().testdir, name, way)
        elif passFail == 'fail':
            if getTestOpts().expect == 'pass' \
               and way not in getTestOpts().expect_fail_for:
                print '*** unexpected failure for', full_name
                t.n_unexpected_failures = t.n_unexpected_failures + 1
                reason = result['reason']
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
    print '*** framework failure for', full_name, reason, ':'
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

def failBecause(reason):
    return {'passFail': 'fail', 'reason': reason}

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

def ghci_script( name, way, script ):
    # filter out -fforce-recomp from compiler_always_flags, because we're
    # actually testing the recompilation behaviour in the GHCi tests.
    flags = filter(lambda f: f != '-fforce-recomp', getTestOpts().compiler_always_flags)
    flags.append(getTestOpts().extra_hc_opts)
    if getTestOpts().outputdir != None:
        flags.extend(["-outputdir", getTestOpts().outputdir])

    # We pass HC and HC_OPTS as environment variables, so that the
    # script can invoke the correct compiler by using ':! $HC $HC_OPTS'
    cmd = "HC='" + config.compiler + "' " + \
          "HC_OPTS='" + join(flags,' ') + "' " + \
          "'" + config.compiler + "'" + \
          ' --interactive -v0 -ignore-dot-ghci ' + \
          join(flags,' ')

    getTestOpts().stdin = script
    return simple_run( name, way, cmd, getTestOpts().extra_run_opts )

# -----------------------------------------------------------------------------
# Compile-only tests

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

def do_compile( name, way, should_fail, top_mod, extra_mods, extra_hc_opts ):
    # print 'Compile only, extra args = ', extra_hc_opts
    pretest_cleanup(name)

    result = extras_build( way, extra_mods, extra_hc_opts )
    if badResult(result):
       return result
    extra_hc_opts = result['hc_opts']

    force = 0
    if extra_mods:
       force = 1
    result = simple_build( name, way, extra_hc_opts, should_fail, top_mod, 0, 1, force)

    if badResult(result):
        return result

    # the actual stderr should always match the expected, regardless
    # of whether we expected the compilation to fail or not (successful
    # compilations may generate warnings).

    if getTestOpts().with_namebase == None:
        namebase = name
    else:
        namebase = getTestOpts().with_namebase

    (platform_specific, expected_stderr_file) = platform_wordsize_qualify(namebase, 'stderr')
    actual_stderr_file = qualify(name, 'comp.stderr')

    if not compare_outputs('stderr', \
                           two_normalisers(two_normalisers(getTestOpts().extra_errmsg_normaliser, normalise_errmsg), normalise_whitespace), \
                           expected_stderr_file, actual_stderr_file):
        return failBecause('stderr mismatch')

    # no problems found, this test passed
    return passed()

def compile_cmp_asm( name, way, extra_hc_opts ):
    print 'Compile only, extra args = ', extra_hc_opts
    pretest_cleanup(name)
    result = simple_build( name + '.cmm', way, '-keep-s-files -O ' + extra_hc_opts, 0, '', 0, 0, 0)

    if badResult(result):
        return result

    # the actual stderr should always match the expected, regardless
    # of whether we expected the compilation to fail or not (successful
    # compilations may generate warnings).

    if getTestOpts().with_namebase == None:
        namebase = name
    else:
        namebase = getTestOpts().with_namebase

    (platform_specific, expected_asm_file) = platform_wordsize_qualify(namebase, 'asm')
    actual_asm_file = qualify(name, 's')

    if not compare_outputs('asm', two_normalisers(normalise_errmsg, normalise_asm), \
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
    elif way == 'extcore' or way == 'optextcore' :
        return extcore_run( name, way, extra_hc_opts, 0, top_mod )
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
    return checkStats(stats_file, opts.stats_range_fields
                                , opts.stats_num_fields)

# -----------------------------------------------------------------------------
# Check -t stats info

def checkStats(stats_file, range_fields, num_fields):
    result = passed()
    if len(num_fields) + len(range_fields) > 0:
        f = open(in_testdir(stats_file))
        contents = f.read()
        f.close()

        for (field, (expected, dev)) in range_fields.items():
            m = re.search('\("' + field + '", "([0-9]+)"\)', contents)
            if m == None:
                print 'Failed to find field: ', field
                result = failBecause('no such stats field')
            val = int(m.group(1))

            min = expected * ((100 - float(dev))/100);
            max = expected * ((100 + float(dev))/100);

            if val < min:
                print field, val, 'is more than ' + repr(dev) + '%'
                print 'less than the exepected value', expected
                print 'If this is because you have improved GHC, please'
                print 'update the test so that GHC doesn\'t regress again'
                result = failBecause('stat too good')
            if val > max:
                print field, val, 'is more than ' + repr(dev) + '% greater than the expected value,', expected, max
                result = failBecause('stat not good enough')

        # ToDo: remove all uses of this, and delete it
        for (field, (min, max)) in num_fields.items():
            m = re.search('\("' + field + '", "([0-9]+)"\)', contents)
            if m == None:
                print 'Failed to find field: ', field
                result = failBecause('no such stats field')
            val = int(m.group(1))

            if val < min:
                print field, val, 'is less than minimum allowed', min
                print 'If this is because you have improved GHC, please'
                print 'update the test so that GHC doesn\'t regress again'
                result = failBecause('stat too good')
            if val > max:
                print field, val, 'is more than maximum allowed', max
                result = failBecause('stat not good enough')

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


def simple_build( name, way, extra_hc_opts, should_fail, top_mod, link, addsuf, noforce ):
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
    if len(opts.compiler_stats_num_fields) + len(opts.compiler_stats_range_fields) > 0:
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

    comp_flags = copy.copy(getTestOpts().compiler_always_flags)
    if noforce:
        comp_flags = filter(lambda f: f != '-fforce-recomp', comp_flags)
    if getTestOpts().outputdir != None:
        comp_flags.extend(["-outputdir", getTestOpts().outputdir])

    cmd = 'cd ' + getTestOpts().testdir + " && " + cmd_prefix + "'" \
          + config.compiler + "' " \
          + join(comp_flags,' ') + ' ' \
          + to_do + ' ' + srcname + ' ' \
          + join(config.way_flags[way],' ') + ' ' \
          + extra_hc_opts + ' ' \
          + opts.extra_hc_opts + ' ' \
          + '>' + errname + ' 2>&1'

    result = runCmdFor(name, cmd)

    if result != 0 and not should_fail:
        actual_stderr = qualify(name, 'comp.stderr')
        if_verbose(1,'Compile failed (status ' + `result` + ') errors were:')
        if_verbose_dump(1,actual_stderr)

    # ToDo: if the sub-shell was killed by ^C, then exit

    statsResult = checkStats(stats_file, opts.compiler_stats_range_fields
                                       , opts.compiler_stats_num_fields)

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

def simple_run( name, way, prog, args ):
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
    if len(opts.stats_num_fields) + len(opts.stats_range_fields) > 0:
        args += ' +RTS -V0 -t' + stats_file + ' --machine-readable -RTS'

    if opts.no_stdin:
        stdin_comes_from = ''
    else:
        stdin_comes_from = ' <' + use_stdin

    if opts.combined_output:
        redirection = ' >' + run_stdout \
                    + ' 2>&1'
    else:
        redirection = ' >' + run_stdout \
                    + ' 2>' + run_stderr

    cmd = prog + ' ' + args + ' '  \
        + my_rts_flags + ' '       \
        + stdin_comes_from         \
        + redirection

    if opts.cmd_wrapper != None:
        cmd = opts.cmd_wrapper(cmd);

    cmd = 'cd ' + opts.testdir + ' && ' + cmd

    # run the command
    result = runCmdFor(name, cmd, timeout_multiplier=opts.timeout_multiplier)

    exit_code = result >> 8
    signal    = result & 0xff

    # check the exit code
    if exit_code != opts.exit_code:
        print 'Wrong exit code (expected', opts.exit_code, ', actual', exit_code, ')'
        dump_stdout(name)
        dump_stderr(name)
        return failBecause('bad exit code')

    check_hp = my_rts_flags.find("-h") != -1
    check_prof = my_rts_flags.find("-p") != -1

    if not opts.ignore_output:
        bad_stderr = not opts.combined_output and not check_stderr_ok(name)
        bad_stdout = not check_stdout_ok(name)
        if bad_stderr:
            return failBecause('bad stderr')
        if bad_stdout:
            return failBecause('bad stdout')
        # exit_code > 127 probably indicates a crash, so don't try to run hp2ps.
        if check_hp and (exit_code <= 127 or exit_code == 251) and not check_hp_ok(name):
            return failBecause('bad heap profile')
        if check_prof and not check_prof_ok(name):
            return failBecause('bad profile')

    return checkStats(stats_file, opts.stats_range_fields
                                , opts.stats_num_fields)

def rts_flags(way):
    if (way == ''):
        return ''
    else:
        args = config.way_rts_flags[way]

    if args == []:
        return ''
    else:
        return '+RTS ' + join(args,' ') + ' -RTS'

# -----------------------------------------------------------------------------
# Run a program in the interpreter and check its output

def interpreter_run( name, way, extra_hc_opts, compile_only, top_mod ):
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
        stdin = open(stdin_file, 'r')
        os.system('cat ' + stdin_file + ' >>' + qscriptname)

    script.close()

    flags = copy.copy(getTestOpts().compiler_always_flags)
    if getTestOpts().outputdir != None:
        flags.extend(["-outputdir", getTestOpts().outputdir])

    cmd = "'" + config.compiler + "' " \
          + join(flags,' ') + ' ' \
          + srcname + ' ' \
          + join(config.way_flags[way],' ') + ' ' \
          + extra_hc_opts + ' ' \
          + getTestOpts().extra_hc_opts + ' ' \
          + '<' + scriptname +  ' 1>' + outname + ' 2>' + errname

    if getTestOpts().cmd_wrapper != None:
        cmd = getTestOpts().cmd_wrapper(cmd);

    cmd = 'cd ' + getTestOpts().testdir + " && " + cmd

    result = runCmdFor(name, cmd, timeout_multiplier=getTestOpts().timeout_multiplier)

    exit_code = result >> 8
    signal    = result & 0xff

    # split the stdout into compilation/program output
    split_file(in_testdir(outname), delimiter,
               qualify(name, 'comp.stdout'),
               qualify(name, 'run.stdout'))
    split_file(in_testdir(errname), delimiter,
               qualify(name, 'comp.stderr'),
               qualify(name, 'run.stderr'))

    # check the exit code
    if exit_code != getTestOpts().exit_code:
        print 'Wrong exit code (expected', getTestOpts().exit_code, ', actual', exit_code, ')'
        dump_stdout(name)
        dump_stderr(name)
        return failBecause('bad exit code')

    # ToDo: if the sub-shell was killed by ^C, then exit

    if getTestOpts().ignore_output or (check_stderr_ok(name) and
                                       check_stdout_ok(name)):
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
# Generate External Core for the given program, then compile the resulting Core
# and compare its output to the expected output

def extcore_run( name, way, extra_hc_opts, compile_only, top_mod ):

    depsfilename = qualify(name, 'deps')
    errname = add_suffix(name, 'comp.stderr')
    qerrname = qualify(errname,'')

    hcname = qualify(name, 'hc')
    oname = qualify(name, 'o')

    rm_no_fail( qerrname )
    rm_no_fail( qualify(name, '') )

    if (top_mod == ''):
        srcname = add_hs_lhs_suffix(name)
    else:
        srcname = top_mod

    qcorefilename = qualify(name, 'hcr')
    corefilename = add_suffix(name, 'hcr')
    rm_no_fail(qcorefilename)

    # Generate External Core

    if (top_mod == ''):
        to_do = ' ' + srcname + ' '
    else:
        to_do = ' --make ' + top_mod + ' '

    flags = copy.copy(getTestOpts().compiler_always_flags)
    if getTestOpts().outputdir != None:
        flags.extend(["-outputdir", getTestOpts().outputdir])
    cmd = 'cd ' + getTestOpts().testdir + " && '" \
          + config.compiler + "' " \
          + join(flags,' ') + ' ' \
          + join(config.way_flags[way],' ') + ' ' \
          + extra_hc_opts + ' ' \
          + getTestOpts().extra_hc_opts \
          + to_do \
          + '>' + errname + ' 2>&1'
    result = runCmdFor(name, cmd)

    exit_code = result >> 8

    if exit_code != 0:
         if_verbose(1,'Compiling to External Core failed (status ' + `result` + ') errors were:')
         if_verbose_dump(1,qerrname)
         return failBecause('ext core exit code non-0')

     # Compile the resulting files -- if there's more than one module, we need to read the output
     # of the previous compilation in order to find the dependencies
    if (top_mod == ''):
        to_compile = corefilename
    else:
        result = runCmdFor(name, 'grep Compiling ' + qerrname + ' |  awk \'{print $4}\' > ' + depsfilename)
        deps = open(depsfilename).read()
        deplist = string.replace(deps, '\n',' ');
        deplist2 = string.replace(deplist,'.lhs,', '.hcr');
        to_compile = string.replace(deplist2,'.hs,', '.hcr');

    flags = join(filter(lambda f: f != '-fext-core',config.way_flags[way]),' ')
    if getTestOpts().outputdir != None:
        flags.extend(["-outputdir", getTestOpts().outputdir])

    cmd = 'cd ' + getTestOpts().testdir + " && '" \
          + config.compiler + "' " \
          + join(getTestOpts().compiler_always_flags,' ') + ' ' \
          + to_compile + ' ' \
          + extra_hc_opts + ' ' \
          + getTestOpts().extra_hc_opts + ' ' \
          + flags                   \
          + ' -fglasgow-exts -o ' + name \
          + '>' + errname + ' 2>&1'

    result = runCmdFor(name, cmd)
    exit_code = result >> 8

    if exit_code != 0:
        if_verbose(1,'Compiling External Core file(s) failed (status ' + `result` + ') errors were:')
        if_verbose_dump(1,qerrname)
        return failBecause('ext core exit code non-0')

    # Clean up
    rm_no_fail ( oname )
    rm_no_fail ( hcname )
    rm_no_fail ( qcorefilename )
    rm_no_fail ( depsfilename )

    return simple_run ( name, way, './'+name, getTestOpts().extra_run_opts )

# -----------------------------------------------------------------------------
# Utils

def check_stdout_ok( name ):
   if getTestOpts().with_namebase == None:
       namebase = name
   else:
       namebase = getTestOpts().with_namebase

   actual_stdout_file   = qualify(name, 'run.stdout')
   (platform_specific, expected_stdout_file) = platform_wordsize_qualify(namebase, 'stdout')

   def norm(str):
      if platform_specific:
         return str
      else:
         return normalise_output(str)

   return compare_outputs('stdout', \
                          two_normalisers(norm, getTestOpts().extra_normaliser), \
                          expected_stdout_file, actual_stdout_file)

def dump_stdout( name ):
   print 'Stdout:'
   print read_no_crs(qualify(name, 'run.stdout'))

def check_stderr_ok( name ):
   if getTestOpts().with_namebase == None:
       namebase = name
   else:
       namebase = getTestOpts().with_namebase

   actual_stderr_file   = qualify(name, 'run.stderr')
   (platform_specific, expected_stderr_file) = platform_wordsize_qualify(namebase, 'stderr')

   def norm(str):
      if platform_specific:
         return str
      else:
         return normalise_errmsg(str)

   return compare_outputs('stderr', \
                          two_normalisers(norm, getTestOpts().extra_normaliser), \
                          expected_stderr_file, actual_stderr_file)

def dump_stderr( name ):
   print "Stderr:"
   print read_no_crs(qualify(name, 'run.stderr'))

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
    hp2psCmd = "cd " + getTestOpts().testdir + " && '" + config.hp2ps + "' " + name

    hp2psResult = runCmdExitCode(hp2psCmd)

    actual_ps_file = qualify(name, 'ps')

    if(hp2psResult == 0):
        if (os.path.exists(actual_ps_file)):
            if gs_working:
                gsResult = runCmdExitCode(genGSCmd(actual_ps_file))
                if (gsResult == 0):
                    return (True)
                else:
                    print "hp2ps output for " + name + "is not valid PostScript"
            else: return (True) # assume postscript is valid without ghostscript
        else:
            print "hp2ps did not generate PostScript for " + name
            return (False)
    else:
        print "hp2ps error when processing heap profile for " + name
        return(False)

def check_prof_ok(name):

    prof_file = qualify(name,'prof')

    if not os.path.exists(prof_file):
        print prof_file + " does not exist"
        return(False)

    if os.path.getsize(qualify(name,'prof')) == 0:
        print prof_file + " is empty"
        return(False)

    if getTestOpts().with_namebase == None:
        namebase = name
    else:
        namebase = getTestOpts().with_namebase

    (platform_specific, expected_prof_file) = \
        platform_wordsize_qualify(namebase, 'prof.sample')

    # sample prof file is not required
    if not os.path.exists(expected_prof_file):
        return True
    else:
        return compare_outputs('prof', \
                               two_normalisers(normalise_whitespace,normalise_prof), \
                               expected_prof_file, prof_file)

# Compare expected output to actual output, and optionally accept the
# new output. Returns true if output matched or was accepted, false
# otherwise.
def compare_outputs( kind, normaliser, expected_file, actual_file ):
    if os.path.exists(expected_file):
        expected_raw = read_no_crs(expected_file)
        # print "norm:", normaliser(expected_raw)
        expected_str = normaliser(expected_raw)
        expected_file_for_diff = expected_file
    else:
        expected_str = ''
        expected_file_for_diff = '/dev/null'

    actual_raw = read_no_crs(actual_file)
    actual_str = normaliser(actual_raw)

    if expected_str == actual_str:
        return 1
    else:
        print 'Actual ' + kind + ' output differs from expected:'

        if expected_file_for_diff == '/dev/null':
            expected_normalised_file = '/dev/null'
        else:
            expected_normalised_file = expected_file + ".normalised"
            write_file(expected_normalised_file, expected_str)

        actual_normalised_file = actual_file + ".normalised"
        write_file(actual_normalised_file, actual_str)

        # Ignore whitespace when diffing. We should only get to this
        # point if there are non-whitespace differences
        #
        # Note we are diffing the *actual* output, not the normalised
        # output.  The normalised output may have whitespace squashed
        # (including newlines) so the diff would be hard to read.
        # This does mean that the diff might contain changes that
        # would be normalised away.
        r = os.system( 'diff -uw ' + expected_file_for_diff + \
                               ' ' + actual_file )

        # If for some reason there were no non-whitespace differences,
        # then do a full diff
        if r == 0:
            r = os.system( 'diff -u ' + expected_file_for_diff + \
                                  ' ' + actual_file )

        if config.accept:
            print 'Accepting new output.'
            write_file(expected_file, actual_raw)
            return 1
        else:
            return 0


def normalise_whitespace( str ):
    # Merge contiguous whitespace characters into a single space.
    str = re.sub('[ \t\n]+', ' ', str)
    return str

def normalise_errmsg( str ):
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
    # We sometimes see the name of the integer-gmp package on stderr,
    # but this can change (either the implementation name or the
    # version number), so we canonicalise it here
    str = re.sub('integer-[a-z]+', 'integer-impl', str)
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
    # Remove a .exe extension (for Windows)
    # This can occur in error messages generated by the program.
    str = re.sub('([^\\s])\\.exe', '\\1', str)
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

def if_verbose( n, str ):
    if config.verbose >= n:
        print str

def if_verbose_dump( n, f ):
    if config.verbose >= n:
        try:
            print open(f).read()
        except:
            print ''

def rawSystem(cmd_and_args):
    # We prefer subprocess.call to os.spawnv as the latter
    # seems to send its arguments through a shell or something
    # with the Windows (non-cygwin) python. An argument "a b c"
    # turns into three arguments ["a", "b", "c"].

    # However, subprocess is new in python 2.4, so fall back to
    # using spawnv if we don't have it

    if have_subprocess:
        return subprocess.call(cmd_and_args)
    else:
        return os.spawnv(os.P_WAIT, cmd_and_args[0], cmd_and_args)

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
    if_verbose( 1, cmd )
    r = 0
    if config.os == 'mingw32':
        # On MinGW, we will always have timeout
        assert config.timeout_prog!=''

    if config.timeout_prog != '':
        r = rawSystem([config.timeout_prog, str(config.timeout), cmd])
    else:
        r = os.system(cmd)
    return r << 8

def runCmdFor( name, cmd, timeout_multiplier=1.0 ):
    if_verbose( 1, cmd )
    r = 0
    if config.os == 'mingw32':
        # On MinGW, we will always have timeout
        assert config.timeout_prog!=''
    timeout = int(math.ceil(config.timeout * timeout_multiplier))

    if config.timeout_prog != '':
        if config.check_files_written:
            fn = name + ".strace"
            r = rawSystem(["strace", "-o", fn, "-fF", "-e", "creat,open,chdir,clone,vfork",
                           config.timeout_prog, str(timeout),
                           cmd])
            addTestFilesWritten(name, fn)
            rm_no_fail(fn)
        else:
            r = rawSystem([config.timeout_prog, str(timeout), cmd])
    else:
        r = os.system(cmd)
    return r << 8

def runCmdExitCode( cmd ):
    return (runCmd(cmd) >> 8);


# -----------------------------------------------------------------------------
# checking for files being written to by multiple tests

re_strace_call_end = '(\) += ([0-9]+|-1 E.*)| <unfinished ...>)$'
re_strace_unavailable       = re.compile('^\) += \? <unavailable>$')
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
re_strace_ignore_sigvtalarm = re.compile('^--- SIGVTALRM \(Virtual timer expired\) @ 0 \(0\) ---$')
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
            elif re_strace_unavailable.match(line):
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
            elif re_strace_ignore_sigvtalarm.match(content):
                pass
            elif re_strace_ignore_sigint.match(content):
                pass
            elif re_strace_ignore_sigfpe.match(content):
                pass
            elif re_strace_ignore_sigsegv.match(content):
                pass
            elif re_strace_ignore_sigpipe.match(content):
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
        tests = files_written_not_removed.keys()
        tests.sort()
        for t in tests:
            for f in files_written_not_removed[t]:
                file.write("    " + t + ": " + f + "\n")
        file.write("\n")

    # -----

    if len(bad_file_usages) > 0:
        file.write("\n")
        file.write("\nSome bad file usages:\n")
        tests = bad_file_usages.keys()
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
    print "GhostScript not available for hp2ps tests"

global gs_working
gs_working = 0
if config.have_profiling:
  if config.gs != '':
    resultGood = runCmdExitCode(genGSCmd(config.confdir + '/good.ps'));
    if resultGood == 0:
        resultBad = runCmdExitCode(genGSCmd(config.confdir + '/bad.ps'));
        if resultBad != 0:
            print "GhostScript available for hp2ps tests"
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

def in_testdir( name ):
    return (getTestOpts().testdir + '/' + name)

def qualify( name, suff ):
    return in_testdir(add_suffix(name, suff))


# Finding the sample output.  The filename is of the form
#
#   <test>.stdout[-<compiler>][-<version>][-ws-<wordsize>][-<platform>]
#
# and we pick the most specific version available.  The <version> is
# the major version of the compiler (e.g. 6.8.2 would be "6.8").  For
# more fine-grained control use if_compiler_lt().
#
def platform_wordsize_qualify( name, suff ):

    basepath = qualify(name, suff)

    paths = [(platformSpecific, basepath + comp + vers + ws + plat)
             for (platformSpecific, plat) in [(1, '-' + config.platform),
                                              (1, '-' + config.os),
                                              (0, '')]
             for ws   in ['-ws-' + config.wordsize, '']
             for comp in ['-' + config.compiler_type, '']
             for vers in ['-' + config.compiler_maj_version, '']]

    dir = glob.glob(basepath + '*')
    dir = map (lambda d: normalise_slashes_(d), dir)

    for (platformSpecific, f) in paths:
       if f in dir:
            return (platformSpecific,f)

    return (0, basepath)

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

   rm_no_fail(qualify(name,'comp.stderr'))
   rm_no_fail(qualify(name,'run.stderr'))
   rm_no_fail(qualify(name,'run.stdout'))
   rm_no_fail(qualify(name,'tix'))  # remove the old tix file
   # simple_build zaps the following:
   # rm_nofail(qualify("o"))
   # rm_nofail(qualify(""))
   # not interested in the return code

# -----------------------------------------------------------------------------
# Return a list of all the files ending in '.T' below the directory dir.

def findTFiles(roots):
    return concat(map(findTFiles_,roots))

def findTFiles_(path):
    if os.path.isdir(path):
        paths = map(lambda x, p=path: p + '/' + x, os.listdir(path))
        return findTFiles(paths)
    elif path[-2:] == '.T':
        return [path]
    else:
        return []

# -----------------------------------------------------------------------------
# Output a test summary to the specified file object

def summary(t, file):

    file.write('\n')
    file.write('OVERALL SUMMARY for test run started at ' \
               + t.start_time + '\n'\
               + string.rjust(`t.total_tests`, 8) \
               + ' total tests, which gave rise to\n' \
               + string.rjust(`t.total_test_cases`, 8) \
               + ' test cases, of which\n' \
               + string.rjust(`t.n_framework_failures`, 8) \
               + ' caused framework failures\n' \
               + string.rjust(`t.n_tests_skipped`, 8)
               + ' were skipped\n\n' \
               + string.rjust(`t.n_expected_passes`, 8)
               + ' expected passes\n' \
               + string.rjust(`t.n_missing_libs`, 8)
               + ' had missing libraries\n' \
               + string.rjust(`t.n_expected_failures`, 8) \
               + ' expected failures\n' \
               + string.rjust(`t.n_unexpected_passes`, 8) \
               + ' unexpected passes\n'
               + string.rjust(`t.n_unexpected_failures`, 8) \
               + ' unexpected failures\n'
               + '\n')

    if t.n_unexpected_passes > 0:
        file.write('Unexpected passes:\n')
        printPassingTestInfosSummary(file, t.unexpected_passes)

    if t.n_unexpected_failures > 0:
        file.write('Unexpected failures:\n')
        printFailingTestInfosSummary(file, t.unexpected_failures)

    if config.check_files_written:
        checkForFilesWrittenProblems(file)

def printPassingTestInfosSummary(file, testInfos):
    directories = testInfos.keys()
    directories.sort()
    maxDirLen = max(map ((lambda x : len(x)), directories))
    for directory in directories:
        tests = testInfos[directory].keys()
        tests.sort()
        for test in tests:
           file.write('   ' + directory.ljust(maxDirLen + 2) + test + \
                      ' (' + join(testInfos[directory][test],',') + ')\n')
    file.write('\n')

def printFailingTestInfosSummary(file, testInfos):
    directories = testInfos.keys()
    directories.sort()
    maxDirLen = max(map ((lambda x : len(x)), directories))
    for directory in directories:
        tests = testInfos[directory].keys()
        tests.sort()
        for test in tests:
           reasons = testInfos[directory][test].keys()
           for reason in reasons:
               file.write('   ' + directory.ljust(maxDirLen + 2) + test + \
                          ' [' + reason + ']' + \
                          ' (' + join(testInfos[directory][test][reason],',') + ')\n')
    file.write('\n')

def getStdout(cmd):
    if have_subprocess:
        p = subprocess.Popen(cmd,
                             stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
        (stdout, stderr) = p.communicate()
        r = p.wait()
        if r != 0:
            raise Exception("Command failed: " + str(cmd))
        if stderr != '':
            raise Exception("stderr from command: " + str(cmd))
        return stdout
    else:
        raise Exception("Need subprocess to get stdout, but don't have it")
