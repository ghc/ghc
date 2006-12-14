# 
# (c) Simon Marlow 2002
#

import sys
import os
import string
import re
import traceback
import copy
import glob

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

def expect_broken( bug ):
    return lambda opts, b=bug: _expect_broken (opts, b )

def _expect_broken( opts, bug ):
    opts.expect = 'fail';

# -----

def expect_fail_for( ways ):
    return lambda opts, w=ways: _expect_fail_for( opts, w )

def _expect_fail_for( opts, ways ):
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

def expect_fail_if_platform( plat ):
   return lambda opts, p=plat: _expect_fail_if_platform(opts, p)

def _expect_fail_if_platform( opts, plat ):
    if config.platform == plat:
	opts.expect = 'fail'

def expect_broken_if_platform( bug, plat ):
   return lambda opts, b=bug, p=plat: _expect_broken_if_platform(opts, b, p)

def _expect_broken_if_platform( opts, bug, plat ):
    if config.platform == plat:
	opts.expect = 'fail'
	
# -----

def expect_fail_if_compiler_type( compiler_type ):
   return lambda opts, c=compiler_type: _expect_fail_if_compiler_type(opts, c)

def _expect_fail_if_compiler_type( opts, compiler_type ):
    if config.compiler_type == compiler_type:
	opts.expect = 'fail'

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

def extra_run_opts( val ):
    return lambda opts, v=val: _extra_run_opts(opts, v);

def _extra_run_opts( opts, v ):
    opts.extra_run_opts = v

def no_clean( opts ):
    opts.cleanup = '';

# -----

def skip_if_no_ghci(opts):
  if not ('ghci' in config.run_ways):
      opts.skip = 1

# ----

def skip_if_fast(opts):
  if config.fast:
      opts.skip = 1

# -----

def skip_if_platform( plat ):
   return lambda opts, p=plat: _skip_if_platform(opts, p)

def _skip_if_platform( opts, plat ):
    if config.platform == plat:
	opts.skip = 1
	
# ---
def alone(opts):
    opts.alone = 1

# ---
def literate( opts ):
    opts.literate = 1;

def c_src( opts ):
    opts.c_src = 1;

# ----
# Function for composing two opt-fns together

def compose( f, g ):
    return lambda opts, f=f, g=g: _compose(opts,f,g)

def _compose( opts, f, g ):    
    f(opts)
    g(opts)

# -----------------------------------------------------------------------------
# The current directory of tests

global testdir
testdir = '.'

def newTestDir( dir ):
    global testdir, thisdir_testopts
    testdir = dir
    # reset the options for this test directory
    thisdir_testopts = copy.copy(default_testopts)

def getTestDir():
    return testdir

# -----------------------------------------------------------------------------
# Actually doing tests

# name  :: String
# setup :: TestOpts -> IO ()  
def test ( name, setup, func, args):
    n = 1
    opts = copy.copy(thisdir_testopts)
    setup(opts)
    if opts.alone:
        n = config.threads

    ok = 0

    if config.use_threads:
        t.thread_pool.acquire()
        try:
            while config.threads<(t.running_threads+n):
                t.thread_pool.wait()
            t.running_threads = t.running_threads+n
            ok=1
            t.thread_pool.release()
            thread.start_new_thread(test_common_thread, (n, name, opts, func, args))
        except:
            if not ok:
                t.thread_pool.release()
    else:
        test_common_work (name, opts, func, args)

if config.use_threads:
    def test_common_thread(n, name, opts, func, args):
        t.lock.acquire()
        try:
            test_common_work(name,opts,func,args)
        finally:
            t.lock.release()
            t.thread_pool.acquire()
            t.running_threads = t.running_threads - n
            t.thread_pool.notify()
            t.thread_pool.release()
    
def test_common_work (name, opts, func, args):
    t.total_tests = t.total_tests+1
    setLocalTestOpts(opts)
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

    t.total_test_cases = t.total_test_cases + len(all_ways)

    ok_way = lambda way: \
        not getTestOpts().skip \
        and (config.only == [] or name in config.only) \
        and (getTestOpts().only_ways == [] or way in getTestOpts().only_ways) \
        and way not in getTestOpts().omit_ways

    # Which ways we are asked to skip
    do_ways = filter (ok_way,all_ways)

    # In fast mode, we skip all but one way
    if config.fast and len(do_ways) > 0:
        do_ways = [do_ways[0]]

    # Run the required tests...
    for way in do_ways:
        do_test (name, way, func, args)

    for way in all_ways:
        if way not in do_ways:
            skiptest (name,way)

    clean(map (lambda suff: name + suff,
              ['', '.exe', '.genscript', '.run.stderr', '.run.stdout',
               '.comp.stderr', '.comp.stdout',
               '.interp.stderr', '.interp.stdout',
               '.hi', '.o', '.prof', '.exe.prof', '.hc', '_stub.h', '_stub.c',
               '_stub.o', '.hp', '.exe.hp', '.ps', '.aux', '.hcr']))

def clean(names):
    clean_full_paths(map (lambda name: in_testdir(name), names))

def clean_o_hi():
    clean_full_paths(glob.glob(in_testdir('*.o')) + glob.glob(in_testdir('*.hi')))

def clean_full_paths(names):
    if getTestOpts().cleanup != '':
        for name in names:
            try:
                os.remove(name)
            except OSError:
                pass

def do_test(name, way, func, args):
    full_name = name + '(' + way + ')'

    try:
        print '=====>', full_name
        
        if config.use_threads:
            t.lock.release()
        try:
            result = apply(func, [name,way] + args)
        finally:
            if config.use_threads:
                t.lock.acquire()
        
        if getTestOpts().expect != 'pass' and getTestOpts().expect != 'fail' or \
           result != 'pass' and result != 'fail':
            framework_fail(full_name)

        if result == 'pass':
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
                if name in t.unexpected_passes:
                    t.unexpected_passes[name].append(way)
                else:
                    t.unexpected_passes[name] = [way]
        else:
            if getTestOpts().expect == 'pass' \
               and way not in getTestOpts().expect_fail_for:
                print '*** unexpected failure for', full_name
                t.n_unexpected_failures = t.n_unexpected_failures + 1
                if name in t.unexpected_failures:
                    t.unexpected_failures[name].append(way)
                else:
                    t.unexpected_failures[name] = [way]
            else:
                t.n_expected_failures = t.n_expected_failures + 1
                if name in t.expected_failures:
                    t.expected_failures[name].append(way)
                else:
                    t.expected_failures[name] = [way]
    except:
        print '*** framework failure for', full_name, ':'
        traceback.print_exc()
        framework_fail(full_name)

def skiptest (name, way):
    # print 'Skipping test \"', name, '\"'
    t.n_tests_skipped = t.n_tests_skipped + 1
    if name in t.tests_skipped:
        t.tests_skipped[name].append(way)
    else:
        t.tests_skipped[name] = [way]

def framework_fail( name ):
    t.n_framework_failures = t.n_framework_failures + 1
    t.framework_failures.append(name)

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
    return simple_run( name, '', cmd, '', 0 )

def run_command_ignore_output( name, way, cmd ):
    return simple_run( name, '', cmd, '', 1 )

# -----------------------------------------------------------------------------
# GHCi tests

def ghci_script( name, way, script ):
    # filter out -no-recomp from compiler_always_flags, becuase we're
    # actually testing the recompilation behaviour in the GHCi tests.
    flags = filter(lambda f: f != '-no-recomp', config.compiler_always_flags)

    # We pass HC and HC_OPTS as environment variables, so that the
    # script can invoke the correct compiler by using ':! $HC $HC_OPTS'
    cmd = "HC='" + config.compiler + "' " + \
          "HC_OPTS='" + join(flags,' ') + "' " + \
          "'" + config.compiler + "'" + \
          ' --interactive -v0 ' + \
          join(flags,' ')

    getTestOpts().stdin = script
    return simple_run( name, way, cmd, getTestOpts().extra_run_opts, 0 )

# -----------------------------------------------------------------------------
# Compile-only tests

def compile( name, way, extra_hc_opts ):
    return do_compile( name, way, 0, '', extra_hc_opts )

def compile_fail( name, way, extra_hc_opts ):
    return do_compile( name, way, 1, '', extra_hc_opts )

def multimod_compile( name, way, top_mod, extra_hc_opts ):
    return do_compile( name, way, 0, top_mod, extra_hc_opts )

def multimod_compile_fail( name, way, top_mod, extra_hc_opts ):
    return do_compile( name, way, 1, top_mod, extra_hc_opts )

def do_compile( name, way, should_fail, top_mod, extra_hc_opts ):
    # print 'Compile only, extra args = ', extra_hc_opts
    pretest_cleanup(name)
    result = simple_build( name, way, extra_hc_opts, should_fail, top_mod, 0 )
    
    if should_fail:
        if result == 0:
            return 'fail'
    else:
        if result != 0:
            return 'fail'

    # the actual stderr should always match the expected, regardless
    # of whether we expected the compilation to fail or not (successful
    # compilations may generate warnings).

    (platform_specific, expected_stderr_file) = platform_wordsize_qualify(name, 'stderr')
    actual_stderr_file = qualify(name, 'comp.stderr')
    actual_stderr = normalise_errmsg(open(actual_stderr_file).read())

    if os.path.exists(expected_stderr_file):
        expected_stderr = normalise_errmsg(open(expected_stderr_file).read())
    else:
        expected_stderr = ''
        expected_stderr_file = ''

    if different_outputs(expected_stderr, actual_stderr):
        print actual_stderr, '\n', expected_stderr
        if not outputs_differ('stderr', expected_stderr_file, actual_stderr_file):
            return 'fail'

    # no problems found, this test passed
    return 'pass'

# -----------------------------------------------------------------------------
# Compile-and-run tests

def compile_and_run( name, way, extra_hc_opts ):
    # print 'Compile and run, extra args = ', extra_hc_opts
    pretest_cleanup(name)

    if way == 'ghci': # interpreted...
        return interpreter_run( name, way, extra_hc_opts, 0, '' )
    elif way == 'extcore' or way == 'optextcore' :
        return extcore_run( name, way, extra_hc_opts, 0, '' )
    else: # compiled...
        result = simple_build( name, way, extra_hc_opts, 0, '', 1 )

        if result != 0:
            return 'fail'

        # we don't check the compiler's stderr for a compile-and-run test
        return simple_run( name, way, './'+name, getTestOpts().extra_run_opts, 0 )


def multimod_compile_and_run( name, way, top_mod, extra_hc_opts ):
    pretest_cleanup(name)

    if way == 'ghci': # interpreted...
        return interpreter_run( name, way, extra_hc_opts, 0, top_mod )
    elif way == 'extcore' or way == 'optextcore' :
        return extcore_run( name, way, extra_hc_opts, 0, top_mod )
    else: # compiled...
        result = simple_build( name, way, extra_hc_opts, 0, top_mod, 1 )

    if result != 0:
        return 'fail'

    # we don't check the compiler's stderr for a compile-and-run test
    return simple_run( name, way, './'+name, getTestOpts().extra_run_opts, 0 )

def multimod_compile_and_run_ignore_output( name, way, top_mod, extra_hc_opts ):
    pretest_cleanup(name)

    if way == 'ghci': # interpreted...
        # not supported: exit code is too difficult to check.
	return 'pass'
    elif way == 'extcore' or way == 'optextcore' :
        return extcore_run( name, way, extra_hc_opts, 0, top_mod )
    else: # compiled...
        result = simple_build( name, way, extra_hc_opts, 0, top_mod, 1 )

    if result != 0:
        return 'fail'

    # we don't check the compiler's stderr for a compile-and-run test
    return simple_run( name, way, './'+name, getTestOpts().extra_run_opts, 1 )

# -----------------------------------------------------------------------------
# Build a single-module program

def simple_build( name, way, extra_hc_opts, should_fail, top_mod, link ):
    errname = add_suffix(name, 'comp.stderr')
    rm_no_fail( errname )
    rm_no_fail( name )
    
    if top_mod != '':
        srcname = top_mod
    else:
        srcname = add_hs_lhs_suffix(name)

    to_do = ''
    if top_mod != '':
        to_do = '--make -o ' + name
    elif link:
        to_do = '-o ' + name
    elif getTestOpts().compile_to_hc:
        to_do = '-C'
    else:
        to_do = '-c' # just compile


    cmd = 'cd ' + testdir + " && '" \
          + config.compiler + "' " \
          + join(config.compiler_always_flags,' ') + ' ' \
          + to_do + ' ' + srcname + ' ' \
          + join(config.way_flags[way],' ') + ' ' \
          + extra_hc_opts + ' ' \
          + getTestOpts().extra_hc_opts + ' ' \
          + '>' + errname + ' 2>&1'

    result = runCmd(cmd)

    if result != 0 and not should_fail:
        actual_stderr = qualify(name, 'comp.stderr')
        if_verbose(1,'Compile failed (status ' + `result` + ') errors were:')
        if_verbose(1,open(actual_stderr).read())

    # ToDo: if the sub-shell was killed by ^C, then exit

    return result

# -----------------------------------------------------------------------------
# Run a program and check its output
#
# If testname.stdin exists, route input from that, else
# from /dev/null.  Route output to testname.run.stdout and 
# testname.run.stderr.  Returns the exit code of the run.

def simple_run( name, way, prog, args, ignore_output_files ):
   # figure out what to use for stdin
   if getTestOpts().stdin != '':
       use_stdin = getTestOpts().stdin
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
   
   my_rts_flags = rts_flags(way)

   cmd = 'cd ' + testdir + ' && ' \
	  + prog + ' ' + args + ' ' \
          + my_rts_flags + ' ' \
          + ' <' + use_stdin \
          + ' >' + run_stdout \
          + ' 2>' + run_stderr

   # run the command
   result = runCmd(cmd)

   exit_code = result >> 8
   signal    = result & 0xff

   # check the exit code
   if exit_code != getTestOpts().exit_code:
       print 'Wrong exit code (expected', getTestOpts().exit_code, ', actual', exit_code, ')'
       return 'fail'

   check_hp = my_rts_flags.find("-h") != -1

   if ignore_output_files or (check_stdout_ok(name) and 
                              check_stderr_ok(name) and
                              (not check_hp or check_hp_ok(name))):
       return 'pass'
   else:
       return 'fail'

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

    cmd = 'cd ' + testdir + " && '" \
          + config.compiler + "' " \
          + join(config.compiler_always_flags,' ') + ' ' \
          + srcname + ' ' \
          + join(config.way_flags[way],' ') + ' ' \
          + extra_hc_opts + ' ' \
          + getTestOpts().extra_hc_opts + ' ' \
          + '<' + scriptname +  ' 1>' + outname + ' 2>' + errname

    result = runCmd(cmd)

    exit_code = result >> 8
    signal    = result & 0xff

    # check the exit code
    if exit_code != getTestOpts().exit_code:
        print 'Wrong exit code (expected', getTestOpts().exit_code, ', actual', exit_code, ')'
        return 'fail'

    # split the stdout into compilation/program output
    split_file(in_testdir(outname), delimiter,
               qualify(name, 'comp.stdout'),
               qualify(name, 'run.stdout'))
    split_file(in_testdir(errname), delimiter,
               qualify(name, 'comp.stderr'),
               qualify(name, 'run.stderr'))

    # ToDo: if the sub-shell was killed by ^C, then exit

    if check_stdout_ok(name) and check_stderr_ok(name):
        return 'pass'
    else:
        return 'fail'


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

    line = infile.readline()
    while (line != ''):
        out2.write(line)
        line = infile.readline()
    
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

    cmd = 'cd ' + testdir + " && '" \
          + config.compiler + "' " \
          + join(config.compiler_always_flags,' ') + ' ' \
          + join(config.way_flags[way],' ') + ' ' \
          + extra_hc_opts + ' ' \
          + getTestOpts().extra_hc_opts \
          + to_do \
          + '>' + errname + ' 2>&1'
    result = runCmd(cmd)

    exit_code = result >> 8

    if exit_code != 0:
         if_verbose(1,'Compiling to External Core failed (status ' + `result` + ') errors were:')
         if_verbose(1,open(qerrname).read())
         return 'fail'

     # Compile the resulting files -- if there's more than one module, we need to read the output
     # of the previous compilation in order to find the dependencies
    if (top_mod == ''):
        to_compile = corefilename
    else:
        result = runCmd('grep Compiling ' + qerrname + ' |  awk \'{print $4}\' > ' + depsfilename)
        deps = open(depsfilename).read()
        deplist = string.replace(deps, '\n',' ');
        deplist2 = string.replace(deplist,'.lhs,', '.hcr');
        to_compile = string.replace(deplist2,'.hs,', '.hcr');
        
    flags = join(filter(lambda f: f != '-fext-core',config.way_flags[way]),' ')
    
    cmd = 'cd ' + testdir + " && '" \
          + config.compiler + "' " \
          + join(config.compiler_always_flags,' ') + ' ' \
          + to_compile + ' ' \
          + extra_hc_opts + ' ' \
          + getTestOpts().extra_hc_opts + ' ' \
          + flags                   \
          + ' -fglasgow-exts -o ' + name \
          + '>' + errname + ' 2>&1'
          
    result = runCmd(cmd)
    exit_code = result >> 8

    if exit_code != 0:
        if_verbose(1,'Compiling External Core file(s) failed (status ' + `result` + ') errors were:')
        if_verbose(1,open(qerrname).read())
        return 'fail'

    # Clean up
    rm_no_fail ( oname )
    rm_no_fail ( hcname )
    rm_no_fail ( qcorefilename )
    rm_no_fail ( depsfilename )
    
    return simple_run ( name, way, './'+name, getTestOpts().extra_run_opts, 0 )

# -----------------------------------------------------------------------------
# Utils

def check_stdout_ok( name ):
   actual_stdout_file   = qualify(name, 'run.stdout')
   (platform_specific, expected_stdout_file) = platform_wordsize_qualify(name, 'stdout')

   if os.path.exists(expected_stdout_file):
       expected_stdout = open(expected_stdout_file).read()
   else:
       expected_stdout = ''
       expected_stdout_file = ''

   if os.path.exists(actual_stdout_file):
       actual_stdout = open(actual_stdout_file).read()
       if not platform_specific:
           actual_stdout = normalise_output(actual_stdout)
   else:
       actual_stdout = ''
       actual_stdout_file = ''

   if different_outputs(actual_stdout, expected_stdout):
       return outputs_differ( 'stdout', expected_stdout_file, actual_stdout_file )
   else:
       return 1

def check_stderr_ok( name ):
   actual_stderr_file   = qualify(name, 'run.stderr')
   (platform_specific, expected_stderr_file) = platform_wordsize_qualify(name, 'stderr')

   if os.path.exists(expected_stderr_file):
       expected_stderr = open(expected_stderr_file).read()
   else:
       expected_stderr = ''
       expected_stderr_file = ''

   if os.path.exists(actual_stderr_file):
       actual_stderr = open(actual_stderr_file).read()
       if not platform_specific:
           actual_stderr = normalise_output(actual_stderr)
   else:
       actual_stderr = ''
       actual_stderr_file = ''

   if different_outputs( actual_stderr,expected_stderr ):
       return outputs_differ( 'stderr', expected_stderr_file, actual_stderr_file )
   else:
       return 1


def check_hp_ok(name):

    # do not qualify for hp2ps because we should be in the right directory
    hp2psCmd = 'cd ' + testdir + ' && ' + config.hp2ps + ' ' + name 

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
            print "hp2ps did not generate PostScript for" + name
            return (False) 
    else:
        print "hp2ps error when processing heap profile for " + name
        return(False)

def different_outputs( str1, str2 ):
   # On Windows, remove '\r' characters from the output
   return re.sub('\r', '', str1) != re.sub('\r', '', str2)

# Output a message indicating that an expected output differed from the
# actual output, and optionally accept the new output.  Returns true
# if the output was accepted, or false otherwise.
def outputs_differ( kind, expected, actual ):
    print 'Actual ' + kind + ' output differs from expected:'

    if expected == '':
        expected1 = '/dev/null'
    else:
        expected1 = expected
        
    if actual == '':
        actual1 = '/dev/null'
    else:
        actual1 = actual
        
    os.system( 'diff -c ' + expected1 + ' ' + actual1 )

    if config.accept:
        if expected == '':
            print '*** cannot accept new output: ' + kind + \
                  ' file does not exist.'
            return 0
        else:
            print 'Accepting new output.'
            os.system( 'cp ' + actual + ' ' + expected )
            return 1


def normalise_errmsg( str ):
    # Merge contiguous whitespace characters into a single space.
    str = re.sub('[ \r\t\n]+', ' ', str)
    # Look for file names and zap the directory part:
    #    foo/var/xyzzy/somefile  -->  somefile
    str = re.sub('([^\\s/]+/)*([^\\s/])', '\\2', str)
    # If somefile ends in ".exe" or ".exe:", zap ".exe" (for Windows)
    #    the colon is there because it appears in error messages; this
    #    hacky solution is used in place of more sophisticated filename
    #    mangling
    str = re.sub('([^\\s])\\.exe', '\\1', str)
    return str

def normalise_output( str ):
    # Remove a .exe extension (for Windows)
    # This can occur in error messages generated by the program.
    str = re.sub('([^\\s])\\.exe', '\\1', str)
    return str

def if_verbose( n, str ):
    if config.verbose >= n:
        print str

# Guess flags suitable for the compiler.
def guess_compiler_flags():
   if config.compiler_type == 'ghc':
       return ['-no-recomp', '-dcore-lint']
   elif config.compiler_type == 'nhc':
       return ['-an-nhc-specific-flag']
   else:
        return []

def runCmd( cmd ):
    if_verbose( 1, cmd )
    r = 0
    if config.platform == 'i386-unknown-mingw32':
	# On MinGW, we will always have timeout
        assert config.timeout_prog!=''
        # We need to add " around the command, because
        # (like Windows programs usually) timeout parses the commandline.
        r = os.spawnl(os.P_WAIT, config.timeout_prog,
		      config.timeout_prog,`config.timeout`,'"%s"'%cmd )
    elif config.timeout_prog!='':
    	r = os.spawnv(os.P_WAIT, config.timeout_prog,
		      [config.timeout_prog,`config.timeout`,cmd] )
    else:
        r = os.system(cmd)
    return r << 8

def runCmdExitCode( cmd ):
    return (runCmd(cmd) >> 8);


# -----------------------------------------------------------------------------
# checking if ghostscript is available for checking the output of hp2ps

def genGSCmd(psfile):
    return (config.gs + ' -dNODISPLAY -dBATCH -dQUIET -dNOPAUSE ' + psfile);

def gsNotWorking():
    global gs_working 
    print "GhostScript not available for hp2ps tests"
    gs_working = 0;

global gs_working
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
    elif getTestOpts().literate:
        return add_suffix(name, 'lhs')
    else:
        return add_suffix(name, 'hs')

def in_testdir( name ):
    return os.path.join(testdir, name)

def qualify( name, suff ):
    return in_testdir(add_suffix(name, suff))

# "foo" -> qualify("foo-platform") if it exists, otherwise
# try qualify("foo-compiler_type"), qualify("foo-ws-wordsize")
# or finally qualify("foo")
def platform_wordsize_qualify( name, suff ):
    path = qualify(name, suff)
    platform_path = path + '-' + config.platform
    compiler_type_path = path + '-' + config.compiler_type
    wordsize_path = path + '-ws-' + config.wordsize
    if os.path.exists(platform_path):
        return (1,platform_path)
    elif os.path.exists(compiler_type_path):
        return (0,compiler_type_path)
    elif os.path.exists(wordsize_path):
        return (0,wordsize_path)
    else:
        return (0,path)

# Clean up prior to the test, so that we can't spuriously conclude
# that it passed on the basis of old run outputs.
def pretest_cleanup(name):
   rm_no_fail(qualify(name,'comp.stderr'))
   rm_no_fail(qualify(name,'run.stderr'))
   rm_no_fail(qualify(name,'run.stdout'))
   rm_no_fail(qualify(name,'tix'))	# remove the old tix file
   # simple_build zaps the following:
   # rm_nofail(qualify("o"))
   # rm_nofail(qualify(""))
   # not interested in the return code

# -----------------------------------------------------------------------------
# Return a list of all the files ending in '.T' below the directory dir.

def findTFiles(path):
    if os.path.isdir(path):
        paths = map(lambda x, p=path: p + '/' + x, os.listdir(path))
        return concat(map(findTFiles, paths))
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
               + string.rjust(`t.n_expected_failures`, 8) \
               + ' expected failures\n' \
               + string.rjust(`t.n_unexpected_passes`, 8) \
               + ' unexpected passes\n'
               + string.rjust(`t.n_unexpected_failures`, 8) \
               + ' unexpected failures\n'
               + '\n')

    if t.n_unexpected_passes > 0:
        file.write('Unexpected passes:\n')
        keys = t.unexpected_passes.keys()
        keys.sort()
        for test in keys:
            file.write('   ' + test + '(' + \
                       join(t.unexpected_passes[test],',') + ')\n')
        file.write('\n')
            
    if t.n_unexpected_failures > 0:
        file.write('Unexpected failures:\n')
        keys = t.unexpected_failures.keys()
        keys.sort()
        for test in keys:
            file.write('   ' + test + '(' + \
                       join(t.unexpected_failures[test],',') + ')\n')
        file.write('\n')
