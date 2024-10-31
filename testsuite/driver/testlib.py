# coding=utf8
#
# (c) Simon Marlow 2002
#

import csv
import io
import shutil
import os
import re
import traceback
import time
import datetime
import copy
import glob
import sys
from math import ceil, trunc, floor, log
from pathlib import Path, PurePath
import collections
import collections.abc
import subprocess

from testglobals import config, ghc_env, default_testopts, brokens, t, \
                        TestRun, TestResult, TestOptions, PerfMetric
from testutil import strip_quotes, lndir, link_or_copy_file, passed, \
                     failBecause, testing_metrics, residency_testing_metrics, \
                     stable_perf_counters, \
                     PassFail, badResult, memoize
from term_color import Color, colored
import testutil
from cpu_features import have_cpu_feature
import perf_notes as Perf
from perf_notes import MetricChange, PerfStat, StatsException, AlwaysAccept, RelativeMetricAcceptanceWindow
extra_src_files = {'T4198': ['exitminus1.c']} # TODO: See #12223

from my_typing import *

from threading import Timer
from collections import OrderedDict

import asyncio
import contextvars

global wantToStop
wantToStop = False

global title_update_timer
title_update_timer = None

# Update terminal title
# useful progress indicator even when make test VERBOSE=1
# Debounce the updates to avoid spamming them.
def update_terminal_title(progress_args):
    global title_update_timer
    if config.supports_colors:
      def worker():
          print("\033]0;{0} {1} of {2} {3}\007".format(*progress_args), end="")
          sys.stdout.flush()
      if title_update_timer:
        title_update_timer.cancel()
      title_update_timer=Timer(0.05,worker)
      title_update_timer.start()

# I have no idea what the type of this is
global thisdir_settings
thisdir_settings = None # type: ignore

def stopNow() -> None:
    global wantToStop
    wantToStop = True

def stopping() -> bool:
    return wantToStop

_all_ways = None

def get_all_ways() -> Set[WayName]:
    global _all_ways
    if _all_ways is None:
        _all_ways = set(config.way_flags.keys())
    return _all_ways

# Options valid for the current test only (these get reset to
# testdir_testopts after each test).

global testopts_ctx_var
testopts_ctx_var = contextvars.ContextVar('testopts_ctx_var') # type: ignore

def getTestOpts() -> TestOptions:
    return testopts_ctx_var.get()

def setLocalTestOpts(opts: TestOptions) -> None:
    global testopts_ctx_var
    testopts_ctx_var.set(opts)

def needsTargetWrapper() -> bool:
    """ Do we need to use a target wrapper? """
    return config.target_wrapper is not None

def isCompilerStatsTest() -> bool:
    opts = getTestOpts()
    return bool(opts.is_compiler_stats_test)

def isGenericStatsTest() -> bool:
    opts = getTestOpts()
    return bool(opts.generic_stats_test)

def isStatsTest() -> bool:
    opts = getTestOpts()
    return opts.is_stats_test

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

# A test with no hadrian built dependencies, including the test compiler
# This is used for linters.
def no_deps( name, opts):
    opts.hadrian_deps = set()
    return;

def skip( name, opts ):
    opts.skip = True

# disable test on JS arch
def js_skip( name, opts ):
    if js_arch():
        skip(name,opts)

# disable test on WASM arch
def wasm_skip( name, opts ):
    if wasm_arch():
        skip(name,opts)

def windows_skip(name,opts):
    if on_windows():
        skip(name,opts)

# expect broken for the JS backend
def js_broken( bug: IssueNumber ):
    if js_arch():
        return expect_broken(bug);
    else:
        return normal;

# expect occasional failures for the JS backend
def js_fragile( bug: IssueNumber ):
    if js_arch():
        return fragile(bug);
    else:
        return normal;

def expect_fail( name, opts ):
    # The compiler, testdriver, OS or platform is missing a certain
    # feature, and we don't plan to or can't fix it now or in the
    # future.
    opts.expect = 'fail';

def no_lint( name, opts ):
   """Disable Core, STG and Cmm lints. Useful when testing compiler perf."""
   opts.compiler_always_flags = \
       [opt for opt in opts.compiler_always_flags \
            if opt not in ['-dcore-lint', '-dstg-lint', '-dcmm-lint']]

def stage1(name, opts):
    # See Note [Why is there no stage1 setup function?]
    framework_fail(name, 'stage1 setup function does not exist',
                   'add your test to testsuite/tests/stage1 instead')

# Note [Why is there no stage1 setup function?]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

def _req_hadrian_deps(name,opts,deps):
    opts.hadrian_deps.update(deps)

def req_hadrian_deps(deps):
    return lambda name, opts: _req_hadrian_deps(name,opts,deps)

# We want the 'docs-haddock' dependency and not just the
# haddock executable since the haddock tests need documentation
# for the boot libraries
def req_haddock( name, opts ):
    _req_hadrian_deps(name,opts,["docs-haddock"])
    if not config.haddock:
        opts.expect = 'missing-lib'
        opts.skip   = True

def req_profiling( name, opts ):
    '''Require the profiling libraries (add 'GhcLibWays += p' to mk/build.mk)'''
    if not config.have_profiling:
        opts.expect = 'fail'

def req_dynamic_lib_support( name, opts ):
    '''
    Require that the platform have shared object support (N.B. this doesn't
    necessary imply that GHC supports the dynamic way).
    '''
    if not config.supports_dynamic_libs:
        opts.expect = 'fail'

def req_dynamic_hs( name, opts ):
    '''
    Require that the GHC supports dynamic linking of Haskell objects on the
    platform
    '''
    if not config.supports_dynamic_hs:
        opts.expect = 'fail'

def req_dynamic_ghc( name, opts ):
    '''
    Require that the GHC is dynamically linked, if static then skip.
    See tests/perf/size/all.T, specifically foo.so tests for use case
    and example
    '''
    if not config.ghc_dynamic:
        skip(name,opts)

def req_interp( name, opts ):
    if not config.have_interp:
        opts.expect = 'fail'

def req_bco( name, opts ):
    '''
    Require support for ByteCode
    '''

    # Requires the interpreter
    req_interp(name, opts)

    # JS backend doesn't support ByteCode
    js_skip(name, opts)

def req_rts_linker( name, opts ):
    if not config.have_RTS_linker:
        opts.expect = 'fail'
    # JS backend doesn't provide the RTS linker
    js_skip(name, opts)

def req_c( name, opts ):
    """
    Mark a test as requiring C source file support
    """
    # JS backend doesn't support C (yet)
    js_skip(name, opts)

def req_cxx( name, opts ):
    """
    Mark a test as requiring C++ source file support
    """
    # JS backend doesn't support C (yet)
    js_skip(name, opts)

def req_cmm( name, opts ):
    """
    Mark a test as requiring Cmm support
    """
    # JS backend doesn't support Cmm
    js_skip(name, opts)
    omit_ghci(name, opts)

def req_ffi_exports( name, opts):
    """
    Mark a test as requiring FFI exports
    """
    # JS backend doesn't support FFI exports (yet)
    js_skip(name, opts)

def req_asm( name, opts):
    """
    Mark a test as requiring LangAsm support
    """
    # JS backend doesn't support asm
    js_skip(name, opts)

def req_th( name, opts ):
    """
    Mark a test as requiring TemplateHaskell. In addition to having interpreter
    support, currently this means that we don't run the test in the profasm when
    when GHC is dynamically-linked since we can't load profiled objects in this
    case.
    """

    # The JS target always supports TH, even in the stage1 compiler
    # However it doesn't support the "Interpreter" yet (GHCi).
    # So specifically enables TH here for JS.
    if js_arch():
        return normal;

    req_interp(name, opts);

    if ghc_dynamic():
        return _omit_ways(name, opts, ['profasm', 'profthreaded'])

def req_plugins( name, opts ):
    """
    Mark a test as requiring GHC plugins. In additional to having
    interpreter support, currently we don't run the test for cross
    GHCs, since plugin support for cross GHC (!7377) is unfinished
    work (#14335) and completely untested in CI, and we don't enable
    the internal-interpreter flag for the ghc library for cross GHC
    anyway.
    """
    req_interp(name, opts)

    if config.cross:
        opts.skip = True

def req_ghc_smp( name, opts ):
    """
    Mark a test as requiring GHC to be linked with an RTS that supports smp.
    """
    if not config.ghc_has_smp:
        opts.skip = True

def req_target_smp( name, opts ):
    """
    Mark a test as requiring smp when run on the target. If the target does
    not support smp, then skip the test. Use this when the
    test needs to run with smp support.
    """
    if not config.target_has_smp:
        opts.skip = True

def req_ghc_with_threaded_rts( name, opts ):
    if not config.ghc_with_threaded_rts:
        opts.skip = True

def req_process( name, opts ):
    if not config.have_process:
        opts.skip = True

def req_host_target_ghc( name, opts ):
    """
    When testing a cross GHC, some test cases require a host GHC as well (e.g.
    for compiling custom Setup.hs). This is not supported yet (#23236), so for
    the time being we skip them when testing cross GHCs. However, for cross targets
    which don't need a target wrapper (e.g. javascript), we can still run these testcases.
    """
    if needsTargetWrapper():
        opts.skip = True

has_ls_files = None

# Check that ls-files works and returns files from the source tree.
# We just check that "hie.yaml" is there because it's top-level (don't have to deal with
# path differences) and quite unique to GHC.
def req_ls_files(name, opts):
    global has_ls_files
    if has_ls_files is None:
        try:
            files = subprocess.check_output(['git', 'ls-files']).splitlines()
            has_ls_files = b"hie.yaml" in files
        except subprocess.CalledProcessError:
            has_ls_files = False

    if not has_ls_files:
        skip(name, opts)

def ignore_stdout(name, opts):
    opts.ignore_stdout = True

def ignore_stderr(name, opts):
    opts.ignore_stderr = True

def ignore_extension(name, opts):
    """
    Some tests generate files that are not expected to be suffixed with an
    extension type, such as .exe on windows. This option allows these tests to
    have finer-grained control over the filename that the testsuite will look
    for. Examples of such tests are hpc tests which expect a .tix extension and
    hp2ps tests which expect .hp. For these tests, on windows and without
    ignoring the extension, the testsuite will look for, e.g., 'foo.exe.tix'
    instead of 'foo.tix'.
    """
    opts.ignore_extension = True

def combined_output( name, opts ):
    opts.combined_output = True

def use_specs( specs ):
    """
    use_specs allows one to override files based on suffixes. e.g. 'stdout',
    'stderr', 'asm', 'prof.sample', etc.

    Example use_specs({'stdout' : 'prof002.stdout'}) to make the test re-use
    prof002.stdout.

    Full Example:
    test('T5889', [only_ways(['normal']), req_profiling,
                   extra_files(['T5889/A.hs', 'T5889/B.hs']),
                   use_specs({'stdout' : 'prof002.stdout'})],
         multimod_compile,
         ['A B', '-O -prof -fno-prof-count-entries -v0'])

    """
    assert isinstance(specs, dict)
    return lambda name, opts, s=specs: _use_specs( name, opts, s )

def _use_specs( name, opts, specs ):
    opts.extra_files.extend(specs.values ())
    opts.use_specs = specs

# -----

def _lint_ways(name: TestName, ways: List[WayName]) -> None:
    """ Check that all of the ways in a list are valid. """
    unknown_ways = [way
                    for way in get_all_ways()
                    if way not in get_all_ways()
                    ]
    if len(unknown_ways) > 0:
        framework_fail(name, None, 'Unknown ways: %s' % (unknown_ways,))

def expect_fail_for( ways: List[WayName] ):
    def helper( name: TestName, opts ):
        _lint_ways(name, ways)
        opts.expect_fail_for = ways

    return helper


def expect_broken( bug: IssueNumber ):
    """
    This test is expected not to work due to the indicated issue number.
    """
    def helper( name: TestName, opts ):
        record_broken(name, opts, bug)
        opts.expect = 'fail';

    return helper


def expect_broken_for( bug: IssueNumber, ways: List[WayName] ):
    def helper( name: TestName, opts ):
        _lint_ways(name, ways)
        record_broken(name, opts, bug)
        opts.expect_fail_for = ways

    return helper


def record_broken(name: TestName, opts, bug: IssueNumber):
    me = (bug, opts.testdir, name)
    if not me in brokens:
        brokens.append(me)

def _expect_pass(way):
    # Helper function. Not intended for use in .T files.
    opts = getTestOpts()
    return opts.expect == 'pass' and way not in opts.expect_fail_for

# -----

def fragile( bug: IssueNumber ):
    """
    Indicates that failures of this test should be ignored due to fragility
    documented in the given ticket.
    """
    def helper( name, opts, bug=bug ):
        record_broken(name, opts, bug)
        opts.fragile_ways += config.way_flags.keys()

    return helper

def fragile_for( bug: IssueNumber, ways: List[WayName] ):
    """
    Indicates that failures of this test should be ignored due to fragility in
    the given test ways as documented in the given ticket.
    """
    def helper( name: TestName, opts ):
        _lint_ways(name, ways)
        record_broken(name, opts, bug)
        opts.fragile_ways += ways

    return helper

# -----

def omit_ways( ways: List[WayName] ):
    return lambda name, opts: _omit_ways(name, opts, ways)

def _omit_ways( name: TestName, opts, ways: List[WayName] ):
    _lint_ways(name, ways)
    opts.omit_ways += ways

omit_ghci = omit_ways([WayName('ghci'), WayName('ghci-opt')])

# -----

def only_ways( ways: List[WayName] ):
    def helper( name: TestName, opts ):
        _lint_ways(name, ways)
        opts.only_ways = ways

    return helper

only_ghci = only_ways([WayName('ghci'), WayName('ghci-opt')])

# -----

def valid_way( way: WayName ) -> bool:
    if way in {'ghci', 'ghci-opt', 'ghci-ext'}:
        return config.have_RTS_linker
    if way == 'ghci-ext-prof':
        return config.have_RTS_linker and config.have_profiling
    return True

def extra_ways( ways: List[WayName] ):
    def helper( name: TestName, opts ):
        _lint_ways(name, ways)
        opts.extra_ways = [way for way in ways if valid_way(way)]

    return helper


# -----

def set_stdin( file ):
   return lambda name, opts, f=file: _set_stdin(name, opts, f);

def _set_stdin( name, opts, f ):
   opts.stdin = f

# -----

def exit_code( val: int ):
    return lambda name, opts, v=val: _exit_code(name, opts, v);

def _exit_code( name, opts, v ):
    opts.exit_code = v

def signal_exit_code( val: int ):
    if opsys('solaris2'):
        return exit_code( val )
    elif arch('wasm32'):
        # wasmtime always exits with 1 when wasm program exits with a
        # non-zero exit code
        return exit_code(1)
    else:
        # When application running on Linux receives fatal error
        # signal, then its exit code is encoded as 128 + signal
        # value. See http://www.tldp.org/LDP/abs/html/exitcodes.html
        # I assume that Mac OS X behaves in the same way at least Mac
        # OS X builder behavior suggests this.
        return exit_code( val+128 )

# -----

def pre_cmd_timeout_multiplier( val: float ):
    return lambda name, opts, v=val: _pre_cmd_timeout_multiplier(name, opts, v)

def _pre_cmd_timeout_multiplier( name, opts, v ):
    opts.pre_cmd_timeout_multiplier = v

def compile_timeout_multiplier( val: float ):
    return lambda name, opts, v=val: _compile_timeout_multiplier(name, opts, v)

def _compile_timeout_multiplier( name, opts, v ):
    opts.compile_timeout_multiplier = v

def run_timeout_multiplier( val: float ):
    return lambda name, opts, v=val: _run_timeout_multiplier(name, opts, v)

def _run_timeout_multiplier( name, opts, v ):
    opts.run_timeout_multiplier = v

# -----

def extra_run_opts( val ):
    return lambda name, opts, v=val: _extra_run_opts(name, opts, v);

def _extra_run_opts( name, opts, v ):
    opts.extra_run_opts += " " + v

# -----

def extra_hc_opts( val ):
    return lambda name, opts, v=val: _extra_hc_opts(name, opts, v);

def _extra_hc_opts( name, opts, v ):
    opts.extra_hc_opts += " " + v

# -----

def extra_clean( files ):
    # TODO. Remove all calls to extra_clean.
    return lambda _name, _opts: None

def extra_files(files):
    return lambda name, opts: _extra_files(name, opts, files)

def _extra_files(name, opts, files):
    opts.extra_files.extend(files)

# Record the size of a specific file
def collect_size ( deviation, path ):
    return collect_size_func(deviation, lambda: path)

def collect_size_func ( deviation, path_func ):
    return collect_generic_stat ( 'size', deviation, lambda way: os.path.getsize(in_testdir(path_func())) )

def get_dir_size(path):
    total = 0
    try:
        with os.scandir(path) as it:
            for entry in it:
                if entry.is_file():
                    total += entry.stat().st_size
                elif entry.is_dir():
                    total += get_dir_size(entry.path)
        return total
    except FileNotFoundError:
        print("Exception: Could not find: " + path)

def collect_size_dir ( deviation, path ):
    return collect_size_dir_func ( deviation, lambda: path )

# Like collect_size_dir but the path is passed as a function which can be evaluated later.
def collect_size_dir_func( deviation, path_func ):
    return collect_generic_stat ( 'size', deviation, lambda way: get_dir_size(path_func()) )

# Read a number from a specific file
def stat_from_file ( metric, deviation, path ):
    def read_file (way):
        with open(in_testdir(path)) as f:
            return int(f.read())
    return collect_generic_stat ( metric, deviation, read_file )


# Define a set of generic stat tests
def collect_generic_stats ( metric_info ):
    def f(name, opts, f=metric_info):
        return _collect_generic_stat(name, opts, metric_info)
    return f

# wrap the call to collect_size_dir with path_from_ghcPkg in a function. Python
# is call-by-value so if we placed the call in an all.T file then the python
# interpreter would evaluate the call to path_from_ghcPkg
def collect_size_ghc_pkg (deviation, library):
    return collect_size_dir_func(deviation, lambda: path_from_ghcPkg(library, "library-dirs"))

# same for collect_size and find_so
def collect_object_size (deviation, library, use_non_inplace=False):
    if use_non_inplace:
        return collect_size_func(deviation, lambda: find_non_inplace_so(library))
    else:
        return collect_size_func(deviation, lambda: find_so(library))

def path_from_ghcPkg (library, field):
    """Find the field as a path for a library via a call to ghc-pkg. This is a
    testsuite wrapper around a call to ghc-pkg field {library} {field}.
    """

    ### example output from ghc-pkg:
    ###  $ ./ghc-pkg field Cabal library-dirs
    ###    library-dirs: /home/doyougnu/programming/haskell/ghc/_build/stage1/lib/../lib/x86_64-linux-ghc-9.11.20240424/Cabal-3.11.0.0-inplace
    ###    so we split the string and drop the 'library-dirs'
    ghcPkgCmd = fr"{config.ghc_pkg} field {library} {field}"

    try:
        result = subprocess.run(ghcPkgCmd, capture_output=True, shell=True)

        # check_returncode throws an exception if the return code is not 0.
        result.check_returncode()

        # if we get here then the call worked and we have the path we split by
        # whitespace and then return the path which becomes the second element
        # in the array
        return re.split(r'\s+', result.stdout.decode("utf-8"))[1]
    except Exception as e:
        message = f"""
        Attempt to find {field} of {library} using ghc-pkg failed.
        ghc-pkg path: {config.ghc_pkg}
        error" {e}
        """
        print(message)


def _find_so(lib, directory, in_place):
    """Find a shared object file (.so) for lib in directory. We deliberately
    keep the regex simple, just removing the ghc version and project version.
    Example:

    _find_so("Cabal-syntax-3.11.0.0", path-from-ghc-pkg, True) ==>
    /builds/ghc/ghc/_build/install/lib/ghc-9.11.20240410/lib/x86_64-linux-ghc-9.11.20240410/libHSCabal-syntax-3.11.0.0-inplace-ghc9.11.20240410.so

    For a release build the filename replaces "inplace" for a hash (765d in
    this case): libHSCabal-3.12.0.0-765d-ghc9.11.20240508.so
    """

    # produce the suffix for the CI operating system
    suffix = "so"
    # config.os is host os
    if not config.cross:
        if config.os == "mingw32":
            suffix = "dll"
        elif config.os == "darwin":
            suffix = "dylib"

    # Most artfacts are of the form foo-inplace, or foo-<hash> for release
    # builds, except for the rts.
    # "\d+(\.\d+)+" matches version numbers, such as 3.12.0.0 in the above example
    # "\w+"         matches the hash on release builds or "inplace", such as 765d in the example
    # "ghc\S+"      matches the ghc build name: ghc9.11.20240508
    if in_place:
        to_match = r'libHS{}-\d+(\.\d+)+-[a-z0-9]+-ghc\S+\.' + suffix
    else:
        to_match = r'libHS{}-\d+(\.\d+)+-ghc\S+\.' + suffix

    matches = []
    # wrap this in some exception handling, hadrian test will error out because
    # these files don't exist yet, so we pass when this occurs
    try:
        for f in os.listdir(directory):
            if f.endswith(suffix):
                pattern = re.compile(to_match.format(re.escape(lib)))
                match   = re.match(pattern, f)
                if match:
                    matches.append(match.group())
        return os.path.join(directory, matches[0])
    except:
        failBecause('Could not find shared object file: ' + lib)

def find_so(lib):
    return _find_so(lib,path_from_ghcPkg(lib, "dynamic-library-dirs"),True)

def find_non_inplace_so(lib):
    return _find_so(lib,path_from_ghcPkg(lib, "dynamic-library-dirs"),False)


def collect_generic_stat ( metric, deviation: Optional[int], get_stat: Callable[[WayName], str]):
    """
    Define a generic stat test, which computes the statistic by calling the function
    given as the third argument.

    If no deviation is given, the test cannot fail, but the metric will be recorded nevertheless.
    """
    return collect_generic_stats ( { metric: { 'deviation': deviation, 'current': get_stat } } )

def _collect_generic_stat(name : TestName, opts, metric_infos):


    # Add new stats to the stat list
    opts.generic_stats_test.update(metric_infos)

    # Add the way to determine the baseline
    for (metric, info) in metric_infos.items():
        def baselineByWay(way, target_commit, metric=metric):
            return Perf.baseline_metric( \
                              target_commit, name, config.test_env, metric, way, \
                              config.baseline_commit )
        opts.generic_stats_test[metric]["baseline"] = baselineByWay


# -----

# Defaults to "test everything, and only break on extreme cases"
#
# The inputs to this function are slightly interesting:
# metric can be either:
#     - 'all', in which case all 3 possible metrics are collected and compared.
#     - The specific metric one wants to use in the test.
#     - A set of the metrics one wants to use in the test.
#
# Deviation defaults to 20% because the goal is correctness over performance.
# The testsuite should avoid breaking when there is not an actual error.
# Instead, the testsuite should notify of regressions in a non-breaking manner.
#
# collect_compiler_stats is used when the metrics collected are about the compiler.
# collect_stats is used in the majority case when the metrics to be collected
# are about the performance of the runtime code generated by the compiler.
def collect_compiler_stats(metric='all',deviation=20):
    def f(name, opts, m=metric, d=deviation):
        no_lint(name, opts)
        return _collect_stats(name, opts, m, d, None, True)
    return f

def collect_stats(metric='all', deviation=20, static_stats_file=None):
    return lambda name, opts, m=metric, d=deviation, s=static_stats_file: _collect_stats(name, opts, m, d, s)

def statsFile(comp_test: bool, name: str) -> str:
    return name + ('.comp' if comp_test else '') + '.stats'

def perfStatsFile(comp_test: bool, name: str) -> str:
    return name + ('.comp' if comp_test else '') + '.perf.csv'

# This is an internal function that is used only in the implementation.
# 'is_compiler_stats_test' is somewhat of an unfortunate name.
# If the boolean is set to true, it indicates that this test is one that
# measures the performance numbers of the compiler.
# As this is a fairly rare case in the testsuite, it defaults to false to
# indicate that it is a 'normal' performance test.
def _collect_stats(name: TestName, opts, metrics, deviation: Optional[int],
                    static_stats_file: Optional[Union[Path,str]],
                    is_compiler_stats_test: bool = False, is_compiler_perf_test: bool = False) -> None:
    if not re.match('^[0-9]*[a-zA-Z][a-zA-Z0-9._-]*$', name):
        failBecause('This test has an invalid name.')

    if is_compiler_perf_test and config.perf_path is None:
        # If we are doing a 'perf' run but no 'perf' is configured,
        # don't try to read the results.
        # This is a bit weird, though.
        return

    # Normalize metrics to a list of strings.
    if isinstance(metrics, str):
        if metrics == 'all':
            metrics = testing_metrics()
        else:
            metrics = { metrics }

    opts.is_stats_test = True
    if is_compiler_stats_test:
        opts.is_compiler_stats_test = True
        tag = 'compile_time'
    else:
        tag = 'runtime'

    # Compiler performance numbers change when debugging is on, making the results
    # useless and confusing. Therefore, skip if debugging is on.
    if config.compiler_debugged and is_compiler_stats_test:
        opts.skip = True

    # If there are any residency testing metrics then turn on RESIDENCY_OPTS
    if residency_testing_metrics() & metrics:
        if is_compiler_stats_test:
            _extra_hc_opts(name, opts, RESIDENCY_OPTS)
        else:
            _extra_run_opts(name, opts, RESIDENCY_OPTS)

    # How to read the result of the performance test
    def read_stats_file(way, metric_name):
        # Confusingly compile time ghci tests are actually runtime tests, so we have
        # to go and look for the name.stats file rather than name.comp.stats file.
        compiler_stats_test = is_compiler_stats_test and not (way == "ghci" or way == "ghci-opt")

        if static_stats_file:
            stats_file = in_statsdir(static_stats_file)
        else:
            stats_file = Path(in_testdir(statsFile(compiler_stats_test, name)))


        try:
            stats_file_contents = stats_file.read_text()
        except IOError as e:
            raise StatsException(str(e))
        field_match = re.search('\\("' + metric_name + '", "([0-9]+)"\\)', stats_file_contents)
        if field_match is None:
            print('Failed to find metric: ', metric_name)
            raise StatsException("No such metric")
        else:
            val = field_match.group(1)
            assert val is not None
            return int(val)

    # How to read the result of the performance test
    def read_perf_stats_file(way, metric_name):
        FIELDS = ['value','unit','event','runtime','percent']
        # Confusingly compile time ghci tests are actually runtime tests, so we have
        # to go and look for the name.stats file rather than name.comp.stats file.
        compiler_stats_test = is_compiler_stats_test and not (way == "ghci" or way == "ghci-opt")

        perf_stats_file = Path(in_testdir(perfStatsFile(compiler_stats_test, name)))
        perf_metrics = {}
        try:
            perf_csv_lines = perf_stats_file.read_text().splitlines()
            # Output looks like:
            # """
            # # Started on <date>
            #
            # <value>,<unit>,<event>,<runtime>,<percent>,...
            # """
            #
            # Ignore empty lines and lines starting with '#'
            perf_csv = [l for l in perf_csv_lines if l and not l.startswith('#')]

            perf_stats_csv_reader = csv.DictReader(perf_csv, fieldnames=FIELDS, delimiter=";", quotechar="\"")
            for fields in perf_stats_csv_reader:
                perf_metrics[fields['event']] = float(fields['value'])

        except IOError as e:
            raise StatsException(str(e))

        val = perf_metrics[metric_name]
        if val is None:
            print('Failed to find metric: ', metric_name)
            raise StatsException("No such metric")
        else:
            assert val is not None
            return int(val)

    collect_stat = {}
    for metric_name in metrics:
        def action_generator(mn):
            read_stats = read_perf_stats_file if is_compiler_perf_test else read_stats_file
            return lambda way: read_stats(way, mn)
        metric = '{}/{}'.format(tag, metric_name)
        collect_stat[metric] = { "deviation": deviation
                                , "current": action_generator(metric_name) }

    _collect_generic_stat(name, opts, collect_stat)

# -----

def when(b: bool, f):
    # When list_brokens is on, we want to see all expect_broken calls,
    # so we always do f
    if b or config.list_broken:
        return f
    else:
        return normal

def unless(b: bool, f):
    return when(not b, f)

def doing_ghci() -> bool:
    return 'ghci' in config.run_ways or 'ghci-opt' in config.run_ways

def ghc_dynamic() -> bool:
    return config.ghc_dynamic

# Symbols have a leading underscore
def leading_underscore() -> bool:
    return config.leading_underscore

# TNTC enabled
def tables_next_to_code() -> bool:
    return config.tables_next_to_code

def fast() -> bool:
    return config.speed == 2

def platform( plat: str ) -> bool:
    return config.platform == plat

KNOWN_OPERATING_SYSTEMS = set([
    'mingw32',
    'freebsd',
    'openbsd',
    'aix',
    'linux',
    'darwin',
    'solaris2',
])

def exe_extension() -> str:
    if wasm_arch():
        return '.wasm'
    elif on_windows():
        return '.exe'
    return ''

def opsys( os: str ) -> bool:
    assert os in KNOWN_OPERATING_SYSTEMS
    return config.os == os

def arch( arch: str ) -> bool:
    return config.arch == arch

def wordsize( ws: int ) -> bool:
    return config.wordsize == str(ws)

def msys( ) -> bool:
    return config.msys

def cygwin( ) -> bool:
    return config.cygwin

def js_arch() -> bool:
    return arch("javascript");

def on_windows() -> bool:
    return config.os == "mingw32"

def wasm_arch() -> bool:
    return arch("wasm32")

def have_vanilla( ) -> bool:
    return config.have_vanilla

def have_ncg( ) -> bool:
    return config.have_ncg

def have_llvm( ) -> bool:
    return config.have_llvm

def have_dynamic( ) -> bool:
    ''' Were libraries built in the dynamic way? '''
    return config.have_dynamic

def have_dynamic_prof( ) -> bool:
    ''' Were libraries built in the profiled dynamic way? '''
    return config.have_profiling_dynamic

''' Were libraries built in the profiled way? '''
def have_profiling( ) -> bool:
    return config.have_profiling

def have_threaded( ) -> bool:
    return config.ghc_with_threaded_rts

def in_tree_compiler( ) -> bool:
    return config.in_tree_compiler

def unregisterised( ) -> bool:
    return config.unregisterised

def compiler_profiled( ) -> bool:
    return config.compiler_profiled

def compiler_debugged( ) -> bool:
    return config.compiler_debugged

def debug_rts( ) -> bool:
    return config.debug_rts

def have_gdb( ) -> bool:
    return config.have_gdb

def have_readelf( ) -> bool:
    return config.have_readelf

def have_fast_bignum( ) -> bool:
    return config.have_fast_bignum

def have_slow_bignum( ) -> bool:
    return not(have_fast_bignum())

def llvm_build ( ) -> bool:
    return config.ghc_built_by_llvm

def have_thread_sanitizer( ) -> bool:
    return config.have_thread_sanitizer


def gcc_as_cmmp() -> bool:
    return config.cmm_cpp_is_gcc

# -----

def collect_compiler_perf(deviation: Optional[int] = None):
    """
    Record stable performance counters using `perf stat` when available.
    """
    return [
        _collect_compiler_perf_counters(stable_perf_counters(), deviation)
    ]

def collect_compiler_perf_counters(counters: List[str], deviation: Optional[int] = None):
    """
    Record the given event counters using `perf stat` when available.
    """
    return [
        _collect_compiler_perf_counters(set(counters), deviation)
    ]

def _collect_compiler_perf_counters(counters: Set[str], deviation: Optional[int] = None):
    def f(name, opts):
        # Slightly hacky, we need the requested perf_counters in 'simple_run'.
        # Thus, we have to globally register these counters
        opts.compiler_perf_counters += list(counters)
        _collect_stats(name, opts, counters, deviation, False, True, True)
    return f


# Note [Measuring residency]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Residency (peak_megabytes_allocated and max_bytes_used) is sensitive
# to when the major GC runs, which makes it inherently inaccurate.
# Sometime an innocuous change somewhere can shift things around such
# that the samples occur at a different time, and the residency
# appears to change (up or down) when the underlying profile hasn't
# really changed. To further minimize this effect we run with a single
# generation (meaning we get a residency sample on every GC) with a small
# allocation area (as suggested in #17387). That's what +RTS -hT -i0 will do.
# If you find that a test is flaky, sampling frequency can be adjusted by
# shrinking the allocation area (+RTS -A64k, for example).
#
# However, please don't just ignore changes in residency.  If you see
# a change in one of these figures, please check whether it is real or
# not as follows:
#
#  * Run the test with old and new compilers, adding +RTS -hT -i0.001
#    (you don't need to compile anything for profiling or enable profiling
#    libraries to get a heap profile).
#  * view the heap profiles, read off the maximum residency.  If it has
#    really changed, then you know there's an issue.
#
# As an example how much of a difference passing these flags make, here's
# an excerpt from CI metrics from the i386 job of !5604. Note that patch
# only passes RESIDENCY_OPTS to a couple more tests, which to be greatly
# affected:
#
#            Test   Metric         value     New value Change
# ----------------------------------------------------------------
#  T11545(normal) ghc/peak          68.2          56.0 -17.9% GOOD
#  T15304(normal) ghc/peak          40.0          42.0  +5.0%
#  T15630(normal) ghc/peak          16.0          15.0  -6.2%
#  T15304(normal) ghc/max     14177634.7    16174668.0 +14.1% BAD
#  T15630(normal) ghc/max      4583039.1     5457248.0 +19.1%

RESIDENCY_OPTS = '+RTS -A256k -i0 -hT -RTS'

# See Note [Measuring residency].
def collect_runtime_residency(tolerance_pct: float):
    return [
        collect_stats(residency_testing_metrics(), tolerance_pct),
    ]

# See Note [Measuring residency].
def collect_compiler_residency(tolerance_pct: float):
    return [
        collect_compiler_stats(residency_testing_metrics(), tolerance_pct)
    ]

def collect_compiler_runtime(tolerance_pct: float):
    return [
        collect_compiler_stats('bytes allocated', tolerance_pct),
        _collect_compiler_perf_counters(stable_perf_counters())
    ]

# ---

def high_memory_usage(name, opts):
    opts.alone = True

    # ThreadSanitizer significantly increases memory footprint; skip
    if have_thread_sanitizer():
        opts.skip = True

# If a test is for a multi-CPU race, then running the test alone
# increases the chance that we'll actually see it.
def multi_cpu_race(name, opts):
    opts.alone = True

# ---
def literate( name, opts ):
    opts.literate = True

def c_src( name, opts ):
    opts.c_src = True
    req_c (name, opts)

def cxx_src( name, opts ):
    opts.cxx_src = True
    req_cxx (name, opts)

def objc_src( name, opts ):
    opts.objc_src = True

def objcxx_src( name, opts ):
    opts.objcxx_src = True

def cmm_src( name, opts ):
    opts.cmm_src = True
    req_cmm(name, opts)

def outputdir( odir ):
    return lambda name, opts, d=odir: _outputdir(name, opts, d)

def _outputdir( name, opts, odir ):
    opts.outputdir = odir;

# ----

def copy_files(name, opts):
    opts.copy_files = True

# ----

def pre_cmd( cmd ):
    return lambda name, opts, c=cmd: _pre_cmd(name, opts, cmd)

def _pre_cmd( name, opts, cmd ):
    opts.pre_cmd = cmd

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

def grep_prof ( needle : str, groups = None ):
    return normalise_prof_fun(grep_errmsg_norm(needle, groups))


def normalise_prof_fun ( *fs):
    return lambda name, opts: _normalise_prof_fun(name, opts, fs)

def _normalise_prof_fun ( name, opts, *fs):
    opts.extra_prof_normaliser = join_normalisers(opts.extra_prof_normaliser, fs)


def filter_stdout_lines( regex ):
    """ Filter lines of stdout with the given regular expression """
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

def check_errmsg(needle):
    def norm(str):
        if needle in str:
            return "%s contained in -ddump-simpl\n" % needle
        else:
            return "%s not contained in -ddump-simpl\n" % needle
    return normalise_errmsg_fun(norm)

# grep_errmsg(regex,[groups])
# If groups are given, return only the matched groups
# that matches the regex.
def grep_errmsg_norm(needle:str, groups = None):

    def get_match(str:str):
        m = re.search(needle,str)
        if m:
            return "".join([m.group(g) for g in groups if m.group(g) is not None])
        else:
            return None

    def norm(str) -> str:
        if groups == None:
            return "".join( filter(lambda l: re.search(needle,l),
                                   str.splitlines(True)))
        else:
            matches = [get_match(x) for x in str.splitlines(True)]
            res = "\n".join([x for x in matches if x])
            return res
    return norm

def grep_errmsg(needle: str, groups = None):
    return normalise_errmsg_fun(grep_errmsg_norm(needle, groups))

def multiline_grep_errmsg(needle):
    def norm(s):
        match = re.search(needle, s)
        return "" if match is None else match.group(0)
    return normalise_errmsg_fun(norm)

def limit_stdout_lines(count):
    return normalise_fun(lambda str: "".join(str.splitlines(True)[:count]))

def normalise_whitespace_fun(f):
    return lambda name, opts: _normalise_whitespace_fun(name, opts, f)

def _normalise_whitespace_fun(name, opts, f):
    opts.whitespace_normaliser = f

def normalise_win32_io_errors(name, opts):
    """
    On Windows we currently have two IO manager implementations: both WinIO IO
    manager and the old POSIX-emulated implementation. These currently differ
    slightly in the error messages that they provide. Normalise these
    differences away, preferring the new WinIO errors.

    This normalization can be dropped when the old IO manager is removed.
    """

    SUBS = [
        ('Bad file descriptor', 'The handle is invalid.'),
        ('Permission denied', 'Access is denied.'),
        ('No such file or directory', 'The system cannot find the file specified.'),
    ]

    def normalizer(s: str) -> str:
        for old,new in SUBS:
            s = s.replace(old, new)

        return s

    if opsys('mingw32'):
        _normalise_fun(name, opts, normalizer)
        _normalise_errmsg_fun(name, opts, normalizer)

def normalise_version_( *pkgs ):
    def normalise_version__( str ):
        # (name)(-version)(-hash)(-components)
        return re.sub('(' + '|'.join(map(re.escape,pkgs)) + r')-[0-9.]+(-[0-9a-zA-Z+]+)?(-[0-9a-zA-Z]+)?',
                      r'\1-<VERSION>-<HASH>', str)
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
            if (isinstance(el, collections.abc.Iterable)
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

def _newTestDir(name: TestName, opts: TestOptions, tempdir, dir):
    testdir = os.path.join('', *(p for p in PurePath(dir).parts if p != '..'))
    opts.srcdir = Path.cwd() / dir
    opts.testdir_raw = Path(os.path.join(tempdir, testdir, name + testdir_suffix))
    opts.compiler_always_flags = config.compiler_always_flags

# -----------------------------------------------------------------------------
# Actually doing tests

parallelTests = []
aloneTests = []
allTestNames = set([])  # type: Set[TestName]

async def runTest(sem, opts, name: TestName, func, args):
    if sem is None:
        return await test_common_work(name, opts, func, args)
    async with sem:
        return await test_common_work(name, opts, func, args)

# name  :: String
# setup :: [TestOpt] -> IO ()
def test(name: TestName,
         setup: "Callable[[List[TestOptions]], None]",
         func, args) -> None:
    global aloneTests
    global parallelTests
    global allTestNames
    global thisdir_settings
    if name in allTestNames:
        framework_fail(name, None, 'There are multiple tests with this name')
    if not re.match('^[0-9]*[a-zA-Z][a-zA-Z0-9._-]*$', name):
        framework_fail(name, None, 'This test has an invalid name')

    if config.run_only_some_tests:
        if name not in config.only:
            return
        else:
            # Note [Mutating config.only]
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

    if name in config.broken_tests:
        myTestOpts.expect = 'fail'

    thisTest = lambda sem: runTest(sem, myTestOpts, name, func, args)
    if myTestOpts.alone:
        aloneTests.append(thisTest)
    else:
        parallelTests.append(thisTest)
    allTestNames.add(name)

def get_package_cache_timestamp() -> float:
    if config.package_conf_cache_file is None:
        return 0.0
    else:
        try:
            return config.package_conf_cache_file.stat().st_mtime
        except:
            return 0.0

do_not_copy = ('.hi', '.o', '.dyn_hi'
              , '.dyn_o', '.out'
              ,'.hi-boot', '.o-boot') # 12112

async def test_common_work(name: TestName, opts,
                     func, args) -> None:
    try:
        t.total_tests += 1
        setLocalTestOpts(opts)

        package_conf_cache_file_start_timestamp = get_package_cache_timestamp()

        # All the ways we might run this test
        if func == compile or func == multimod_compile:
            all_ways = config.compile_ways
        elif func in [compile_and_run, multi_compile_and_run, multimod_compile_and_run]:
            all_ways = config.run_ways
        elif func == ghci_script:
            if config.have_interp:
                all_ways = [WayName('ghci'), WayName('ghci-opt')]
            else:
                all_ways = []
            if needsTargetWrapper():
                opts.skip = True
        elif func in [makefile_test, run_command]:
            # Note [Makefile tests are supposed to be run in all ways]
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Makefile tests aren't necessarily runtime or compile-time
            # specific. Assume we can run them in all ways. See #16042 for what
            # happened previously.
            #
            # For example, the WASM test environment requires a target wrapper to run tests
            # which is why Makefile tests are skipped by default. For cases where the
            # target wrapper is actually not needed we can trigger Makefile tests to run
            # by using something like `pre_cmd('$MAKE -s --no-print-directory...`.
            # Examples of this can be found throughout the code.
            #
            # Additionally, it is useful to set `multimod_compile` as the running mode
            # because it provides enough flexibility to specify source names to compile
            # without wasting time on running.
            #
            # `ignore_stdout` and `ignore_stderr` could also be helpful in cases where
            # all you need is to compare the exit code with 0.
            all_ways = config.compile_ways + config.run_ways
            if needsTargetWrapper():
                opts.skip = True
        else:
            all_ways = [WayName('normal')]

        # A test itself can request extra ways by setting opts.extra_ways
        all_ways = list(OrderedDict.fromkeys(all_ways + [way for way in opts.extra_ways if way not in all_ways]))

        t.total_test_cases += len(all_ways)

        only_ways = getTestOpts().only_ways
        ok_way = lambda way: \
            not getTestOpts().skip \
            and (only_ways is None
                 or (only_ways is not None and way in only_ways)) \
            and (config.cmdline_ways == [] or way in config.cmdline_ways) \
            and (not (config.skip_perf_tests and isStatsTest())) \
            and (not (config.only_perf_tests and not isStatsTest())) \
            and way not in getTestOpts().omit_ways

        # Which ways we are asked to skip
        do_ways = list(filter (ok_way,all_ways))

        # Only run all ways in slow mode.
        # See Note [validate and testsuite speed] in `validate`
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
        files = set(f for f in os.listdir(str(opts.srcdir))
                       if f.startswith(name) and not f == name and
                          not f.endswith(testdir_suffix) and
                          not os.path.splitext(f)[1] in do_not_copy)
        for filename in (opts.extra_files + extra_src_files.get(name, [])):
            if filename.startswith('/'):
                framework_fail(name, None,
                    'no absolute paths in extra_files please: ' + filename)

            elif '*' in filename:
                # Don't use wildcards in extra_files too much, as
                # globbing is slow.
                files.update(str(Path(f).relative_to(opts.srcdir))
                             for f in glob.iglob(str(in_srcdir(filename))))

            elif filename:
                files.add(filename)

            else:
                framework_fail(name, None, 'extra_file is empty string')

        # If we are only reporting hadrian dependencies, then skip the test
        # and add its dependencies to the global set
        if do_ways and config.only_report_hadrian_deps:
            do_ways = []
            config.hadrian_deps |= getTestOpts().hadrian_deps

        # Run the required tests...
        for way in do_ways:
            if stopping():
                break
            try:
                await do_test(name, way, func, args, files)
            except KeyboardInterrupt:
                stopNow()
            except Exception as e:
                traceback.print_exc()
                framework_fail(name, way, traceback.format_exc())

        t.n_tests_skipped += len(set(all_ways) - set(do_ways))
        if getTestOpts().expect == 'missing-lib': t.n_missing_libs += 1

        if config.cleanup and do_ways:
            try:
                cleanup()
            except Exception as e:
                framework_fail(name, None, 'Unhandled exception during cleanup: ' + str(e))

        package_conf_cache_file_end_timestamp = get_package_cache_timestamp();

        if package_conf_cache_file_start_timestamp != package_conf_cache_file_end_timestamp:
            framework_fail(name, None, 'Package cache timestamps do not match: ' + str(package_conf_cache_file_start_timestamp) + ' ' + str(package_conf_cache_file_end_timestamp))

    except Exception as e:
        framework_fail(name, None, 'Unhandled exception: ' + str(e))

async def do_test(name: TestName,
            way: WayName,
            func: Callable[..., Awaitable[PassFail]],
            args,
            files: Set[str]
            ) -> None:
    opts = getTestOpts()

    full_name = name + '(' + way + ')'
    test_n = len(allTestNames)
    progress_args = [ full_name, t.total_tests, test_n,
        [len(t.unexpected_passes),
         len(t.unexpected_failures),
         len(t.framework_failures)]]
    # For n = [0..100] - Report every test
    #     n = [100..1000] - Report every 10 tests
    #     n = [1000...10000] - report every 100 tests
    #     and so on...
    report_every = 10 ** max(0, floor(log(test_n, 10)) - 1)
    if t.total_tests % report_every == 0 and config.verbose <= 2:
        if_verbose(2, "=====> {1} of {2} {3}".format(*progress_args))
    else:
        if_verbose(3, "=====> {0} {1} of {2} {3}".format(*progress_args))

    update_terminal_title(progress_args)

    # Clean up prior to the test, so that we can't spuriously conclude
    # that it passed on the basis of old run outputs.
    cleanup()
    os.makedirs(str(opts.testdir))

    # Link all source files for this test into a new directory in
    # /tmp, and run the test in that directory. This makes it
    # possible to run tests in parallel, without modification, that
    # would otherwise (accidentally) write to the same output file.
    # It also makes it easier to keep the testsuite clean.

    for extra_file in files:
        src = in_srcdir(extra_file)
        dst = in_testdir(os.path.basename(extra_file.rstrip('/\\')))
        force_copy = opts.copy_files or arch("wasm32")
        if src.is_file():
            link_or_copy_file(src, dst, force_copy)
        elif src.is_dir():
            if dst.exists():
                shutil.rmtree(str(dst))
            dst.mkdir()
            lndir(src, dst, force_copy)
        else:
            if not config.haddock and os.path.splitext(extra_file)[1] == '.t':
                # When using a ghc built without haddock support, .t
                # files are rightfully missing. Don't
                # framework_fail. Test will be skipped later.
                pass
            else:
                framework_fail(name, way,
                    'extra_file does not exist: ' + extra_file)

    if func.__name__ == 'run_command' or func.__name__ == 'makefile_test' or opts.pre_cmd:
        # When running 'MAKE' make sure 'TOP' still points to the
        # root of the testsuite.
        src_makefile = in_srcdir('Makefile')
        dst_makefile = in_testdir('Makefile')
        if src_makefile.exists():
            makefile = src_makefile.read_text(encoding='UTF-8')
            makefile = re.sub('TOP=.*', 'TOP=%s' % config.top, makefile, 1)
            dst_makefile.write_text(makefile, encoding='UTF-8')

    if opts.pre_cmd:
        stdout_path = in_testdir(name, 'pre_cmd_stdout')
        stderr_path = in_testdir(name, 'pre_cmd_stderr')
        exit_code = await runCmd('cd "{0}" && {1}'.format(opts.testdir, override_options(opts.pre_cmd)),
                           stdout = stdout_path,
                           stderr = stderr_path,
                           print_output = config.verbose >= 3,
                           timeout_multiplier = opts.pre_cmd_timeout_multiplier
                           )

        # If user used expect_broken then don't record failures of pre_cmd
        if exit_code != 0 and opts.expect not in ['fail']:
            framework_fail(name, way, 'pre_cmd failed: {0}'.format(exit_code))
            if_verbose(1, '** pre_cmd was "{0}".'.format(override_options(opts.pre_cmd)))
            stderr_contents = stderr_path.read_text(encoding='UTF-8', errors='replace')
            stdout_contents = stdout_path.read_text(encoding='UTF-8', errors='replace')
            if_verbose(1, 'stdout: {0}'.format(stdout_contents))
            if_verbose(1, 'stderr: {0}'.format(stderr_contents))
            # Don't continue and try to run the test if the pre_cmd fails.
            return

    result = await func(*[name,way] + args)

    if opts.expect not in ['pass', 'fail', 'missing-lib']:
        framework_fail(name, way, 'bad expected ' + opts.expect)

    directory = re.sub(r'^\.[/\\]', '', str(opts.testdir))

    if way in opts.fragile_ways:
        if_verbose(1, '*** fragile test %s resulted in %s' % (full_name, 'pass' if result.passed else 'fail'))
        if result.passed:
            t.fragile_passes.append(TestResult(directory, name, 'fragile', way))
        else:
            t.fragile_failures.append(TestResult(directory, name, 'fragile', way,
                                                 stdout=result.stdout,
                                                 stderr=result.stderr))
    elif result.passed:
        if _expect_pass(way):
            t.expected_passes.append(TestResult(directory, name, "", way))
            t.n_expected_passes += 1
        else:
            if_verbose(1, '*** unexpected pass for %s' % full_name)
            t.unexpected_passes.append(TestResult(directory, name, 'unexpected', way))
    else:
        if _expect_pass(way):
            reason = result.reason
            tag = result.tag
            if tag == 'stat':
                if_verbose(1, '*** unexpected stat test failure for %s' % full_name)
                t.unexpected_stat_failures.append(TestResult(directory, name, reason, way))
            else:
                if_verbose(1, '*** unexpected failure for %s' % full_name)
                tr = TestResult(directory, name, reason, way,
                                stdout=result.stdout,
                                stderr=result.stderr)
                t.unexpected_failures.append(tr)
        else:
            t.n_expected_failures += 1

# Make is often invoked with -s, which means if it fails, we get
# no feedback at all. This is annoying. So let's remove the option
# if found and instead have the testsuite decide on what to do
# with the output.
def override_options(pre_cmd):
    if config.verbose >= 5 and bool(re.match(r'\$make', pre_cmd, re.I)):
        return pre_cmd.replace(' -s'     , '') \
                      .replace('--silent', '') \
                      .replace('--quiet' , '')

    return pre_cmd

def framework_fail(name: Optional[TestName], way: Optional[WayName], reason: str) -> None:
    opts = getTestOpts()
    # framework_fail can be called before testdir is initialised,
    # so we need to take care not to blow up with the wrong way
    # and report the actual reason for the failure.
    try:
      directory = re.sub(r'^\.[/\\]', '', str(opts.testdir))
    except:
      directory = ''
    full_name = '%s(%s)' % (name, way)
    if_verbose(1, '*** framework failure for %s %s ' % (full_name, reason))
    name2 = name if name is not None else TestName('none')
    way2 = way if way is not None else WayName('none')
    if way not in opts.fragile_ways:
        # If the test is fragile then we rather report this as a fragile test failure
        t.framework_failures.append(TestResult(directory, name2, reason, way2))

def framework_warn(name: TestName, way: WayName, reason: str) -> None:
    opts = getTestOpts()
    directory = re.sub(r'^\.[/\\]', '', str(opts.testdir))
    full_name = name + '(' + way + ')'
    if_verbose(1, '*** framework warning for %s %s ' % (full_name, reason))
    t.framework_warnings.append(TestResult(directory, name, reason, way))

# -----------------------------------------------------------------------------
# Generic command tests

# A generic command test is expected to run and exit successfully.
#
# The expected exit code can be changed via exit_code() as normal, and
# the expected stdout/stderr are stored in <testname>.stdout and
# <testname>.stderr.  The output of the command can be ignored
# altogether by using the setup function ignore_stdout instead of
# run_command.

async def run_command( name, way, cmd ):
    return await simple_run( name, '', override_options(cmd), '' )

async def makefile_test( name, way, target=None ):
    if target is None:
        target = name

    cmd = '$MAKE -s --no-print-directory {target}'.format(target=target)
    return await run_command(name, way, cmd)

# -----------------------------------------------------------------------------
# GHCi tests

async def ghci_script( name, way, script):
    flags = ' '.join(get_compiler_flags())
    way_flags = ' '.join(config.way_flags[way])

    # We pass HC and HC_OPTS as environment variables, so that the
    # script can invoke the correct compiler by using ':! $HC $HC_OPTS'
    cmd = ('HC={{compiler}} HC_OPTS="{flags}" {{compiler}} {way_flags} {flags}'
          ).format(flags=flags, way_flags=way_flags)
      # NB: put way_flags before flags so that flags in all.T can override others

    getTestOpts().stdin = script
    return await simple_run( name, way, cmd, getTestOpts().extra_run_opts )

# -----------------------------------------------------------------------------
# Compile-only tests

async def compile( name, way, extra_hc_opts ):
    return await do_compile( name, way, False, None, [],  [], extra_hc_opts )

async def compile_artifact( name, way, extra_hc_opts ):
    # We suppress stderr so that the link output isn't compared
    return await do_compile( name, way, False, None, [], [], extra_hc_opts, should_link=True, compare_stderr=False )

async def compile_fail( name, way, extra_hc_opts ):
    return await do_compile( name, way, True, None, [], [], extra_hc_opts )

async def backpack_typecheck( name, way, extra_hc_opts ):
    return await do_compile( name, way, False, None, [], [], "-fno-code -fwrite-interface " + extra_hc_opts, backpack=True )

async def backpack_typecheck_fail( name, way, extra_hc_opts ):
    return await do_compile( name, way, True, None, [], [], "-fno-code -fwrite-interface " + extra_hc_opts, backpack=True )

async def backpack_compile( name, way, extra_hc_opts ):
    return await do_compile( name, way, False, None, [], [], extra_hc_opts, backpack=True )

async def backpack_compile_fail( name, way, extra_hc_opts ):
    return await do_compile( name, way, True, None, [], [], extra_hc_opts, backpack=True )

async def multimod_compile( name, way, top_mod, extra_hc_opts ):
    return await do_compile( name, way, False, top_mod, [], [], extra_hc_opts )

async def multimod_compile_fail( name, way, top_mod, extra_hc_opts ):
    return await do_compile( name, way, True, top_mod, [], [], extra_hc_opts )

async def multimod_compile_filter( name, way, top_mod, extra_hc_opts, filter_with, suppress_stdout=True ):
    return await do_compile( name, way, False, top_mod, [], [], extra_hc_opts, filter_with=filter_with, suppress_stdout=suppress_stdout )

async def multiunit_compile( name, way, units, extra_hc_opts ):
    return await do_compile( name, way, False, None, [], units, extra_hc_opts )

async def multiunit_compile_fail( name, way, units, extra_hc_opts ):
    return await do_compile( name, way, True, None, [], units, extra_hc_opts )

async def multi_compile( name, way, top_mod, extra_mods, extra_hc_opts ):
    return await do_compile( name, way, False, top_mod, extra_mods, [], extra_hc_opts)

async def multi_compile_fail( name, way, top_mod, extra_mods, extra_hc_opts ):
    return await do_compile( name, way, True, top_mod, extra_mods, [], extra_hc_opts)

async def make_depend( name, way, mods, extra_hc_opts ):
    return await do_compile( name, way, False,  ' '.join(mods), [], [], extra_hc_opts, mode = '-M')

async def do_compile(name: TestName,
               way: WayName,
               should_fail: bool,
               top_mod: Optional[Path],
               extra_mods: List[str],
               units: List[str],
               extra_hc_opts: str,
               should_link=False,
               compare_stderr=True,
               **kwargs
               ) -> PassFail:
    # print 'Compile only, extra args = ', extra_hc_opts

    result = await extras_build( way, extra_mods, extra_hc_opts )
    if badResult(result):
       return result

    assert result.hc_opts is not None
    extra_hc_opts = result.hc_opts

    result = await simple_build(name, way, extra_hc_opts, should_fail, top_mod, units, should_link, True, **kwargs)

    if badResult(result):
        return result

    # the actual stderr should always match the expected, regardless
    # of whether we expected the compilation to fail or not (successful
    # compilations may generate warnings).

    expected_stderr_file = find_expected_file(name, 'stderr')
    actual_stderr_file = add_suffix(name, 'comp.stderr')
    diff_file_name = in_testdir(add_suffix(name, 'comp.diff'))

    if compare_stderr and not await compare_outputs(way, 'stderr',
                           join_normalisers(getTestOpts().extra_errmsg_normaliser,
                                            normalise_errmsg),
                           expected_stderr_file, actual_stderr_file,
                           diff_file=diff_file_name,
                           whitespace_normaliser=getattr(getTestOpts(),
                                                         "whitespace_normaliser",
                                                         normalise_whitespace)):
        stderr = diff_file_name.read_text()
        diff_file_name.unlink()
        return failBecause('stderr mismatch', stderr=stderr)

    opts = getTestOpts()
    if isGenericStatsTest():
        statsResult = check_generic_stats(TestName(name), way, opts.generic_stats_test)
        if badResult(statsResult):
            return statsResult

    # no problems found, this test passed
    return passed()

async def compile_cmp_asm(name: TestName,
                    way: WayName,
                    ext: str,
                    extra_hc_opts: str
                    ) -> PassFail:
    if extra_hc_opts:
        print('Compile only, extra args = ', extra_hc_opts)
    result = await simple_build(name + '.' + ext, way, '-keep-s-files -O ' + extra_hc_opts, False, None, [], False, False)

    if badResult(result):
        return result

    # the actual stderr should always match the expected, regardless
    # of whether we expected the compilation to fail or not (successful
    # compilations may generate warnings).

    expected_asm_file = find_expected_file(name, 'asm')
    actual_asm_file = add_suffix(name, 's')

    if not await compare_outputs(way, 'asm',
                           join_normalisers(normalise_errmsg, normalise_asm),
                           expected_asm_file, actual_asm_file):
        return failBecause('asm mismatch')

    # no problems found, this test passed
    return passed()

async def compile_grep_asm(name: TestName,
                     way: WayName,
                     ext: str,
                     is_substring: bool,
                     extra_hc_opts: str
                     ) -> PassFail:
    if extra_hc_opts:
        print('Compile and grep asm, extra args = ', extra_hc_opts)
    result = await simple_build(name + '.' + ext, way, '-keep-s-files -O ' + extra_hc_opts, False, None, [], False, False)

    if badResult(result):
        return result

    expected_pat_file = find_expected_file(name, 'asm')
    actual_asm_file = add_suffix(name, 's')

    if not grep_output(join_normalisers(normalise_errmsg),
                       expected_pat_file, actual_asm_file,
                       is_substring):
        return failBecause('asm mismatch')

    # no problems found, this test passed
    return passed()

async def compile_grep_core(name: TestName,
                      way: WayName,
                      extra_hc_opts: str
                      ) -> PassFail:
    if extra_hc_opts:
        print('Compile only, extra args = ', extra_hc_opts)
    result = await simple_build(name + '.hs', way, '-ddump-to-file -dsuppress-all -ddump-simpl -O ' + extra_hc_opts, False, None, [], False, False)

    if badResult(result):
        return result

    expected_pat_file = find_expected_file(name, 'substr-simpl')
    actual_core_file = add_suffix(name, 'dump-simpl')

    if not grep_output(join_normalisers(normalise_errmsg),
                       expected_pat_file, actual_core_file):
        return failBecause('simplified core mismatch')

    # no problems found, this test passed
    return passed()

# -----------------------------------------------------------------------------
# Compile-and-run tests

async def compile_and_run__(name: TestName,
                      way: WayName,
                      top_mod: Path,
                      extra_mods: List[str],
                      extra_hc_opts: str,
                      backpack: bool=False,
                      compile_stderr: bool=False,
                      use_extension: bool=True
                      ) -> PassFail:
    # print 'Compile and run, extra args = ', extra_hc_opts

    result = await extras_build( way, extra_mods, extra_hc_opts )
    if badResult(result):
       return result
    assert result.hc_opts is not None
    extra_hc_opts = result.hc_opts
    assert extra_hc_opts is not None

    if way.startswith('ghci'): # interpreted...
        return await interpreter_run(name, way, extra_hc_opts, top_mod)
    else: # compiled...
        result = await simple_build(name, way, extra_hc_opts, False, top_mod, [], True, True, backpack = backpack)
        if badResult(result):
            return result

        if compile_stderr:
            expected_stderr_file = find_expected_file(name, 'ghc.stderr')
            actual_stderr_file = add_suffix(name, 'comp.stderr')
            diff_file_name = in_testdir(add_suffix(name, 'comp.diff'))

            if not await compare_outputs(way, 'stderr',
                           join_normalisers(getTestOpts().extra_errmsg_normaliser,
                                            normalise_errmsg),
                           expected_stderr_file, actual_stderr_file,
                           diff_file=diff_file_name,
                           whitespace_normaliser=getattr(getTestOpts(),
                                                         "whitespace_normaliser",
                                                         normalise_whitespace)):
             stderr = diff_file_name.read_text()
             diff_file_name.unlink()
             return failBecause('ghc.stderr mismatch', stderr=stderr)

        opts = getTestOpts()
        extension = exe_extension() if not opts.ignore_extension else ""

        cmd = './' + name + extension

        # we don't check the compiler's stderr for a compile-and-run test
        return await simple_run( name, way, cmd, getTestOpts().extra_run_opts )

async def compile_and_run( name, way, extra_hc_opts ):
    return await compile_and_run__( name, way, None, [], extra_hc_opts)

async def backpack_run( name, way, extra_hc_opts ):
    return await compile_and_run__( name, way, None, [], extra_hc_opts, backpack=True )

async def multimod_compile_and_run( name, way, top_mod, extra_hc_opts ):
    return await compile_and_run__( name, way, top_mod, [], extra_hc_opts)

async def multi_compile_and_run( name, way, top_mod, extra_mods, extra_hc_opts ):
    return await compile_and_run__( name, way, top_mod, extra_mods, extra_hc_opts)

async def warn_and_run( name, way, extra_hc_opts ):
    return await compile_and_run__( name, way, None, [], extra_hc_opts, compile_stderr = True)

async def static_stats( name, way ):
    opts = getTestOpts()
    return check_generic_stats(name, way, opts.generic_stats_test)

def metric_dict(name, way, metric, value) -> PerfStat:
    return Perf.PerfStat(
        test_env = config.test_env,
        test     = name,
        way      = way,
        metric   = metric,
        value    = value)



def check_generic_stats(name, way, get_stats):
    for (metric, gen_stat) in get_stats.items():
        res = report_stats(name, way, metric, gen_stat)
        if badResult(res):
            return res
    return passed()

def report_stats(name, way, metric, gen_stat):
    try:
        actual_val = gen_stat['current'](way)
    # Metrics can exit early by throwing a StatsException with the failure string.
    except StatsException as e:
        return failBecause(e.args[0])

    head_commit = Perf.commit_hash(GitRef('HEAD')) if Perf.inside_git_repo() else None
    if head_commit is None:
        return passed()

    result = passed()
    # Store the metric so it can later be stored in a git note.
    perf_stat = metric_dict(name, way, metric, actual_val)

    # If this is the first time running the benchmark, then pass.
    baseline = gen_stat['baseline'](way, head_commit) \
        if Perf.inside_git_repo() else None
    if baseline is None:
        metric_result = passed()
        perf_change = MetricChange.NewMetric
    else:
        deviation = gen_stat["deviation"]
        if deviation:
            tolerance_metric = RelativeMetricAcceptanceWindow(deviation)
        else:
            tolerance_metric = AlwaysAccept()
        (perf_change, metric_result) = Perf.check_stats_change(
            perf_stat,
            baseline,
            tolerance_metric,
            config.allowed_perf_changes,
            config.verbose >= 4)

    t.metrics.append(PerfMetric(change=perf_change, stat=perf_stat, baseline=baseline))

    # If any metric fails then the test fails.
    # Note, the remaining metrics are still run so that
    # a complete list of changes can be presented to the user.
    if not metric_result.passed:
        if config.ignore_perf_increases and perf_change == MetricChange.Increase:
            metric_result = passed()
        elif config.ignore_perf_decreases and perf_change == MetricChange.Decrease:
            metric_result = passed()

        result = metric_result
    return result

# -----------------------------------------------------------------------------
# Build a single-module program

async def extras_build(way: WayName, extra_mods, extra_hc_opts) -> PassFail:
    for mod, opts in extra_mods:
        result = await simple_build(mod, way, opts + ' ' + extra_hc_opts,
                should_fail=False,
                top_mod=None,
                units=[],
                link=False,
                addsuf=False)
        if not (mod.endswith('.hs') or mod.endswith('.lhs')):
            extra_hc_opts += ' %s' % Path(mod).with_suffix('.o')
        if badResult(result):
            return result

    return passed(hc_opts=extra_hc_opts)

async def simple_build(name: Union[TestName, str],
                 way: WayName,
                 extra_hc_opts: str,
                 should_fail: bool,
                 top_mod: Optional[Path],
                 units: List[str],
                 link: bool,
                 addsuf: bool,
                 backpack: bool = False,
                 suppress_stdout: bool = False,
                 filter_with: str = '',
                 # Override auto-detection of whether to use --make or -c etc.
                 mode: Optional[str] = None) -> Any:
    opts = getTestOpts()

    # Redirect stdout and stderr to the same file
    stdout = in_testdir(name, 'comp.stderr')
    stderr = subprocess.STDOUT if not suppress_stdout else None

    if top_mod is not None:
        srcname = top_mod
    elif addsuf:
        if backpack:
            srcname = add_suffix(name, 'bkp')
        else:
            srcname = add_hs_lhs_suffix(name)
    else:
        srcname = Path(name)

    if mode is not None:
        to_do = mode
    elif top_mod is not None:
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
    elif len(units) > 0:
        to_do = '--make'
        for u in units:
            to_do = to_do + ' -unit @%s' % u
    else:
        to_do = '-c' # just compile

    if isCompilerStatsTest():
        stats_file = statsFile(True, name)
        # Set a bigger chunk size to reduce variation due to additional under/overflowing
        # The tests are attempting to test how much work the compiler is doing by proxy of
        # bytes allocated. The additional allocations caused by stack overflow can cause
        # spurious failures if you trip over the limit (see #23439)
        extra_hc_opts += ' +RTS -kc128k -kb16k -V0 -t' + stats_file + ' --machine-readable -RTS'
    if backpack:
        extra_hc_opts += ' -outputdir ' + name + '.out'

    # Required by GHC 7.3+, harmless for earlier versions:
    if (getTestOpts().c_src or
        getTestOpts().cxx_src or
        getTestOpts().objc_src or
        getTestOpts().objcxx_src):
        extra_hc_opts += ' -no-hs-main '

    if getTestOpts().compile_cmd_prefix == '':
        cmd_prefix = ''
    else:
        cmd_prefix = getTestOpts().compile_cmd_prefix + ' '

    flags = ' '.join(get_compiler_flags() + config.way_flags[way])

    cmd = ('{cmd_prefix} '
           '{{compiler}} {to_do} {srcname} {flags} {extra_hc_opts}'
          ).format(**locals())

    if filter_with != '':
        cmd = cmd + ' | ' + filter_with

    output_file = perfStatsFile(True, name)

    exit_code = await runCmdPerf(
            opts.compiler_perf_counters,
            cmd,
            output_file,
            working_dir=opts.testdir,
            stdin=None, stdout=stdout, stderr=stderr,
            timeout_multiplier=opts.compile_timeout_multiplier)

    actual_stderr_path = in_testdir(name, 'comp.stderr')

    if exit_code != 0 and not should_fail:
        if config.verbose >= 1 and _expect_pass(way):
            print('Compile failed (exit code {0}) errors were:'.format(exit_code))
            dump_file(actual_stderr_path)

    # ToDo: if the sub-shell was killed by ^C, then exit

    if should_fail:
        if exit_code == 0:
            stderr_contents = actual_stderr_path.read_text(encoding='UTF-8', errors='replace')
            return failBecause('exit code 0', stderr=stderr_contents)
    else:
        if exit_code != 0:
            stderr_contents = actual_stderr_path.read_text(encoding='UTF-8', errors='replace')
            return failBecause('exit code non-0', stderr=stderr_contents)

    return passed()

# -----------------------------------------------------------------------------
# Run a program and check its output
#
# If testname.stdin exists, route input from that, else
# from /dev/null.  Route output to testname.run.stdout and
# testname.run.stderr.  Returns the exit code of the run.

async def simple_run(name: TestName, way: WayName, prog: str, extra_run_opts: str) -> Any:
    opts = getTestOpts()

    # figure out what to use for stdin
    if opts.stdin:
        stdin_arg = in_testdir(opts.stdin) # type: Optional[Path]
    elif in_testdir(name, 'stdin').exists():
        stdin_arg = in_testdir(name, 'stdin')
    else:
        stdin_arg = None

    stdout_arg = in_testdir(name, 'run.stdout')
    if opts.combined_output:
        stderr_arg = subprocess.STDOUT # type: Union[int, Path]
    else:
        stderr_arg = in_testdir(name, 'run.stderr')

    my_rts_flags = rts_flags(way)

    # Collect runtime stats if necessary:
    # isStatsTest and not isCompilerStatsTest():
    #   assume we are running a ghc compiled program. Collect stats.
    # isStatsTest and way == 'ghci':
    #   assume we are running a program via ghci. Collect stats
    stats_file = None # type: Optional[str]
    if isStatsTest() and (not isCompilerStatsTest() or way == 'ghci' or way == 'ghci-opt'):
        stats_file = statsFile(False, name)
        stats_args = ' +RTS -V0 -t' + stats_file + ' --machine-readable -RTS'
    else:
        stats_args = ''

    # Put extra_run_opts last: extra_run_opts('+RTS foo') should work.
    args = [prog, stats_args, my_rts_flags, extra_run_opts]
    if opts.target_wrapper is not None:
        args = [opts.target_wrapper] + args
    elif config.target_wrapper is not None:
        args = [config.target_wrapper] + args
    cmd = ' '.join(args)

    if opts.cmd_wrapper is not None:
        cmd = opts.cmd_wrapper(cmd)

    output_file = perfStatsFile(False, name)

    # run the command
    exit_code = await runCmdPerf(opts.compiler_perf_counters, cmd, output_file,
        working_dir=opts.testdir,
        stdin=stdin_arg, stdout=stdout_arg, stderr=stderr_arg,
        timeout_multiplier=opts.run_timeout_multiplier)

    # check the exit code
    if exit_code != opts.exit_code:
        if config.verbose >= 1 and _expect_pass(way):
            print('Wrong exit code for ' + name + '(' + way + ')' + '(expected', opts.exit_code, ', actual', exit_code, ')')
            dump_stdout(name)
            dump_stderr(name)
        message = format_bad_exit_code_message(exit_code)
        return failBecause(message)

    if not (opts.ignore_stderr or await stderr_ok(name, way) or opts.combined_output):
        return failBecause('bad stderr',
                           stderr=read_stderr(name),
                           stdout=read_stdout(name))
    if not (opts.ignore_stdout or await stdout_ok(name, way)):
        return failBecause('bad stdout',
                           stderr=read_stderr(name),
                           stdout=read_stdout(name))

    check_hp = '-hT' in my_rts_flags and opts.check_hp
    check_prof = '-p' in my_rts_flags

    # exit_code > 127 probably indicates a crash, so don't try to run hp2ps.
    if check_hp and (exit_code <= 127 or exit_code == 251) and not await check_hp_ok(name):
        return failBecause('bad heap profile')
    if check_prof and not await check_prof_ok(name, way):
        return failBecause('bad profile')

    # Check the results of stats tests
    if isGenericStatsTest():
        statsResult = check_generic_stats(TestName(name), way, opts.generic_stats_test)
        if badResult(statsResult):
            return statsResult

    return passed()

def rts_flags(way: WayName) -> str:
    args = config.way_rts_flags.get(way, [])
    return '+RTS {0} -RTS'.format(' '.join(args)) if args else ''

# -----------------------------------------------------------------------------
# Run a program in the interpreter and check its output

async def interpreter_run(name: TestName,
                    way: WayName,
                    extra_hc_opts: str,
                    top_mod: Path
                    ) -> PassFail:
    opts = getTestOpts()

    stdout = in_testdir(name, 'interp.stdout')
    stderr = in_testdir(name, 'interp.stderr')
    script = in_testdir(name, 'genscript')

    if opts.combined_output:
        framework_fail(name, WayName('unsupported'),
                       'WAY=ghci[-opt] and combined_output together is not supported')

    if top_mod is None:
        srcname = add_hs_lhs_suffix(name)
    else:
        srcname = Path(top_mod)

    delimiter = '===== program output begins here\n'

    pat = re.compile(r'-main-is (\w+)')
    res = pat.search(extra_hc_opts)
    main_is = res.group(1) if res else "Main.main"
    with script.open('w', encoding='UTF-8') as f:
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
        f.write('GHC.TopHandler.runIOFastExit {main_is} Prelude.>> Prelude.return ()\n'.format(main_is=main_is))

    stdin = in_testdir(opts.stdin if opts.stdin else add_suffix(name, 'stdin'))
    if stdin.exists():
        os.system('cat "{0}" >> "{1}"'.format(stdin, script))

    flags = ' '.join(get_compiler_flags() + config.way_flags[way])

    cmd = ('{{compiler}} {srcname} {flags} {extra_hc_opts}'
          ).format(**locals())

    if opts.cmd_wrapper is not None:
        cmd = opts.cmd_wrapper(cmd);

    cmd = 'cd "{opts.testdir}" && {cmd}'.format(**locals())

    exit_code = await runCmd(cmd, script, stdout, stderr, timeout_multiplier=opts.run_timeout_multiplier)

    # split the stdout into compilation/program output
    split_file(stdout, delimiter,
               in_testdir(name, 'comp.stdout'),
               in_testdir(name, 'run.stdout'))
    split_file(stderr, delimiter,
               in_testdir(name, 'comp.stderr'),
               in_testdir(name, 'run.stderr'))

    # check the exit code
    if exit_code != getTestOpts().exit_code:
        if config.verbose >= 1 and _expect_pass(way):
            print('Wrong exit code for ' + name + '(' + way + ') (expected', getTestOpts().exit_code, ', actual', exit_code, ')')
            dump_stdout(name)
            dump_stderr(name)
        message = format_bad_exit_code_message(exit_code)
        return failBecause(message,
                           stderr=read_stderr(name),
                           stdout=read_stdout(name))

    # ToDo: if the sub-shell was killed by ^C, then exit

    if not (opts.ignore_stderr or await stderr_ok(name, way)):
        if _expect_pass(way):
            dump_stderr_for('comp', name)
        return failBecause('bad stderr',
                           stderr=read_stderr(name),
                           stdout=read_stdout(name))
    elif not (opts.ignore_stdout or await stdout_ok(name, way)):
        if _expect_pass(way):
            dump_stderr_for('comp', name)
        return failBecause('bad stdout',
                           stderr=read_stderr(name),
                           stdout=read_stdout(name))
    else:
        return passed()

def split_file(in_fn: Path, delimiter: str, out1_fn: Path, out2_fn: Path):
    # See Note [Universal newlines].
    with in_fn.open('r', encoding='utf8', errors='replace', newline=None) as infile:
        with out1_fn.open('w', encoding='utf8', newline='') as out1:
            with out2_fn.open('w', encoding='utf8', newline='') as out2:
                line = infile.readline()
                while re.sub(r'^\s*','',line) != delimiter and line != '':
                    out1.write(line)
                    line = infile.readline()

                line = infile.readline()
                while line != '':
                    out2.write(line)
                    line = infile.readline()

# -----------------------------------------------------------------------------
# Utils
def get_compiler_flags() -> List[str]:
    opts = getTestOpts()

    flags = copy.copy(opts.compiler_always_flags)

    flags.append(opts.extra_hc_opts)

    if opts.outputdir is not None:
        flags.extend(["-outputdir", opts.outputdir])

    return flags

async def stdout_ok(name: TestName, way: WayName) -> bool:
   actual_stdout_file = add_suffix(name, 'run.stdout')
   expected_stdout_file = find_expected_file(name, 'stdout')

   extra_norm = join_normalisers(normalise_output, getTestOpts().extra_normaliser)

   check_stdout = getTestOpts().check_stdout
   if check_stdout is not None:
      actual_stdout_path = in_testdir(actual_stdout_file)
      return check_stdout(actual_stdout_path, extra_norm)

   return await compare_outputs(way, 'stdout', extra_norm,
                          expected_stdout_file, actual_stdout_file)

def read_stdout( name: TestName ) -> str:
    path = in_testdir(name, 'run.stdout')
    if path.exists():
        return path.read_text(encoding='UTF-8')
    else:
        return ''

def dump_stdout( name: TestName ) -> None:
    s = read_stdout(name).strip()
    if s:
        print("Stdout (", name, "):")
        safe_print(s)

async def stderr_ok(name: TestName, way: WayName) -> bool:
   actual_stderr_file = add_suffix(name, 'run.stderr')
   expected_stderr_file = find_expected_file(name, 'stderr')

   return await compare_outputs(way, 'stderr',
                          join_normalisers(normalise_errmsg, getTestOpts().extra_errmsg_normaliser), \
                          expected_stderr_file, actual_stderr_file,
                          whitespace_normaliser=normalise_whitespace)

def read_comp_stderr( name: TestName ) -> str:
    path = in_testdir(name, 'comp.stderr')
    if path.exists():
        return path.read_text(encoding='UTF-8')
    else:
        return ''

def read_stderr_for( phase: str, name: TestName ) -> str:
    path = in_testdir(name, phase + '.stderr')
    if path.exists():
        return path.read_text(encoding='UTF-8')
    else:
        return ''

def read_stderr( name: TestName ) -> str:
    return read_stderr_for('run', name)

def dump_stderr_for( phase: str, name: TestName ) -> None:
    s = read_stderr_for(phase, name).strip()
    if s:
        print("Stderr ", phase, " (", name, "):")
        safe_print(s)

def dump_stderr( name: TestName ) -> None:
    dump_stderr_for('comp', name)
    dump_stderr_for('run', name)

def read_no_crs(f: Path) -> str:
    s = ''
    try:
        # See Note [Universal newlines].
        with f.open('r', encoding='utf8', errors='replace', newline=None) as h:
            s = h.read()
    except Exception:
        # On Windows, if the program fails very early, it seems the
        # files stdout/stderr are redirected to may not get created
        pass
    return s

def write_file(f: Path, s: str) -> None:
    # See Note [Universal newlines].
    with f.open('w', encoding='utf8', newline='') as h:
        h.write(s)

# Note [Universal newlines]
# ~~~~~~~~~~~~~~~~~~~~~~~~~
# We don't want to write any Windows style line endings ever, because
# it would mean that `make accept` would touch every line of the file
# when switching between Linux and Windows.
#
# Furthermore, when reading a file, it is convenient to translate all
# Windows style endings to '\n', as it simplifies searching or massaging
# the content.
#
# Solution: use `Path.open` instead of `open`
#  * when reading: use newline=None to translate '\r\n' to '\n'
#  * when writing: use newline='' to not translate '\n' to '\r\n'
#
# See https://docs.python.org/3/library/pathlib.html#pathlib.Path.open
#
# This should work with both python2 and python3, and with both mingw*
# as msys2 style Python.
#
# Do note that Path.open returns unicode strings. So we have to specify
# the expected encoding. But there is at least one file which is not
# valid utf8 (decodingerror002.stdout). Solution: use errors='replace'.
# Another solution would be to open files in binary mode always, and
# operate on bytes.

async def check_hp_ok(name: TestName) -> bool:
    opts = getTestOpts()

    # do not qualify for hp2ps because we should be in the right directory
    hp2psCmd = 'cd "{opts.testdir}" && {{hp2ps}} {name}'.format(**locals())

    hp2psResult = await runCmd(hp2psCmd, print_output=True)

    actual_ps_path = in_testdir(name, 'ps')

    if hp2psResult == 0:
        if actual_ps_path.exists():
            if await does_ghostscript_work():
                gsResult = await runCmd(genGSCmd(actual_ps_path))
                if (gsResult == 0):
                    return True
                else:
                    print("hp2ps output for " + name + " is not valid PostScript")
                    return False
            else:
                return True # assume postscript is valid without ghostscript
        else:
            print("hp2ps did not generate PostScript for " + name)
            return  False
    else:
        print("hp2ps error when processing heap profile for " + name)
        return False

async def check_prof_ok(name: TestName, way: WayName) -> bool:
    expected_prof_file = find_expected_file(name, 'prof.sample')
    expected_prof_path = in_testdir(expected_prof_file)

    # Check actual prof file only if we have an expected prof file to
    # compare it with.
    if not expected_prof_path.exists():
        return True

    actual_prof_file = add_suffix(name, 'prof')
    actual_prof_path = in_testdir(actual_prof_file)

    if not actual_prof_path.exists():
        print("%s does not exist" % actual_prof_path)
        return(False)

    if actual_prof_path.stat().st_size == 0:
        print("%s is empty" % actual_prof_path)
        return(False)

    return await compare_outputs(way, 'prof', normalise_prof,
                            expected_prof_file, actual_prof_file,
                            whitespace_normaliser=normalise_whitespace)

# Compare expected output to actual output, and optionally accept the
# new output. Returns true if output matched or was accepted, false
# otherwise. See Note [Output comparison] for the meaning of the
# normaliser and whitespace_normaliser parameters.
async def compare_outputs(
        way: WayName,
        kind: str,
        normaliser: OutputNormalizer,
        expected_file: Path,
        actual_file: Path,
        diff_file: Optional[Path]=None,
        whitespace_normaliser: OutputNormalizer=lambda x:x) -> bool:

    # Respect ignore_stdout and ignore_stderr options
    if kind == 'stderr' and getTestOpts().ignore_stderr:
        return True
    if kind == 'stdout' and getTestOpts().ignore_stdout:
        return True

    expected_path = in_srcdir(expected_file)
    actual_path = in_testdir(actual_file)

    if expected_path.exists():
        expected_str = normaliser(read_no_crs(expected_path))
        # Create the .normalised file in the testdir, not in the srcdir.
        expected_normalised_file = add_suffix(expected_file, 'normalised')
        expected_normalised_path = in_testdir(expected_normalised_file)
    else:
        expected_str = ''
        # See Note [Null device handling]
        expected_normalised_path = Path(os.devnull)

    actual_raw = read_no_crs(actual_path)
    actual_str = normaliser(actual_raw)

    # See Note [Output comparison].
    if whitespace_normaliser(expected_str) == whitespace_normaliser(actual_str):
        return True
    else:
        if config.verbose >= 1 and _expect_pass(way):
            print('Actual ' + kind + ' output differs from expected:')

        # See Note [Null device handling]
        if expected_normalised_path != Path(os.devnull):
            write_file(expected_normalised_path, expected_str)

        actual_normalised_path = add_suffix(actual_path, 'normalised')
        write_file(actual_normalised_path, actual_str)

        if config.verbose >= 1 and _expect_pass(way):
            # See Note [Output comparison].
            r = await runCmd('diff -uw "{0}" "{1}"'.format(null2unix_null(expected_normalised_path),
                                                        actual_normalised_path),
                        stdout=diff_file,
                        print_output=True)

            # If for some reason there were no non-whitespace differences,
            # then do a full diff
            if r == 0:
                r = await runCmd('diff -u "{0}" "{1}"'.format(null2unix_null(expected_normalised_path),
                                                           actual_normalised_path),
                           stdout=diff_file,
                           print_output=True)
        elif diff_file: diff_file.open('ab').close() # Make sure the file exists still as
                                                     # we will try to read it later

        if config.accept and (getTestOpts().expect == 'fail' or
                              way in getTestOpts().expect_fail_for):
            if_verbose(1, 'Test is expected to fail. Not accepting new output.')
            return False
        elif config.accept and actual_raw:
            if config.accept_platform:
                if_verbose(1, 'Accepting new output for platform "'
                              + config.platform + '".')
                expected_path += '-' + config.platform
            elif config.accept_os:
                if_verbose(1, 'Accepting new output for os "'
                              + config.os + '".')
                expected_path += '-' + config.os
            else:
                if_verbose(1, 'Accepting new output.')

            write_file(expected_path, actual_raw)
            return True
        elif config.accept:
            if_verbose(1, 'No output. Deleting "{0}".'.format(expected_path))
            expected_path.unlink()
            return True
        else:
            if config.unexpected_output_dir is not None:
                ghc_root = expected_path.relative_to(config.top.parent)
                out = config.unexpected_output_dir / ghc_root
                out.parent.mkdir(exist_ok=True, parents=True)
                write_file(out, actual_raw)

            return False

# Checks that each line from pattern_file is present in actual_file as
# a substring or regex pattern depending on is_substring.
def grep_output(normaliser: OutputNormalizer, pattern_file, actual_file, is_substring: bool=True):
    expected_path = in_srcdir(pattern_file)
    actual_path = in_testdir(actual_file)

    expected_patterns = read_no_crs(expected_path).strip().split('\n')
    actual_raw = read_no_crs(actual_path)
    actual_str = normaliser(actual_raw)

    success = True
    failed_patterns = []

    def regex_match(pat, actual):
        return re.search(pat, actual) is not None

    def substring_match(pat, actual):
        return pat in actual

    def is_match(pat, actual):
        if is_substring:
            return substring_match(pat, actual)
        else:
            return regex_match(pat, actual)

    for pat in expected_patterns:
        if not is_match(pat, actual_str):
            success = False
            failed_patterns.append(pat)

    if not success:
        print('Actual output does not contain the following patterns:')
        for pat in failed_patterns:
            print(pat)

    return success

# Note [Output comparison]
# ~~~~~~~~~~~~~~~~~~~~~~~~
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
#
# In addition, to aid CI users we will optionally collect all
# of the unexpected output that we encountered in the
# directory at config.unexpected_output_dir. The intent here is for this
# directory to be preserved as a CI artifact which can then
# be downloaded by the user and committed to their branch
# to address CI failures on platforms which they cannot
# test locally.


# Note [Null device handling]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# On windows the null device is 'nul' instead of '/dev/null'.
# This can in principle be easily solved by using os.devnull.
# Not doing so causes issues when python tries to read/write/open
# the null device.
#
# However this still leads to a problem when executing shell
# commands in the msys environment. Which again expect '/dev/null'.
#
# So what we do is use os.devnull and convert it to the string
# '/dev/null' for shell commands which are bound to run in a
# unix-like environment.

def null2unix_null(f: Path) -> str:
    if f == Path(os.devnull):
        return ('/dev/null')
    else:
        return f.as_posix()

def normalise_whitespace(s: str) -> str:
    # Merge contiguous whitespace characters into a single space.
    return ' '.join(s.split())

callSite_re = re.compile(r', called at (.+):[\d]+:[\d]+ in [<>\w\-\.]+:')

def normalise_callstacks(s: str) -> str:
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

tyCon_re = re.compile(r'TyCon\s*\d+\#\#\d?\d?\s*\d+\#\#\d?\d?\s*', flags=re.MULTILINE)

def normalise_type_reps(s: str) -> str:
    """ Normalise out fingerprints from Typeable TyCon representations """
    return re.sub(tyCon_re, 'TyCon FINGERPRINT FINGERPRINT ', s)

def normalise_errmsg(s: str) -> str:
    """Normalise error-messages emitted via stderr"""
    # IBM AIX's `ld` is a bit chatty
    if opsys('aix'):
        s = s.replace('ld: 0706-027 The -x flag is ignored.\n', '')
    # remove " error:" and lower-case " Warning:" to make patch for
    # trac issue #10021 smaller
    s = modify_lines(s, lambda l: re.sub(' error:', '', l))
    s = modify_lines(s, lambda l: re.sub(' Warning:', ' warning:', l))
    s = normalise_callstacks(s)
    s = normalise_type_reps(s)

    # normalise slashes to minimise Windows/Unix filename differences,
    # but don't normalize backslashes in chars
    s = re.sub(r"(?<!('|‘))\\", '/', s)

    # Normalize the name of the GHC executable. Specifically,
    # this catches the cases that:
    #
    # * In cross-compilers ghc's executable name may include
    #   a target prefix (e.g. `aarch64-linux-gnu-ghc`)
    # * On Windows the executable name may mention the
    #   versioned name (e.g. `ghc-9.2.1`)
    s = re.sub(Path(config.compiler).name + ':', 'ghc:', s)

    # If somefile ends in ".exe" or ".exe:", zap ".exe" (for Windows)
    #    the colon is there because it appears in error messages; this
    #    hacky solution is used in place of more sophisticated filename
    #    mangling
    s = re.sub(r'([^\s])\.exe', r'\1', s)
    # Same thing for .wasm modules generated by the Wasm backend
    s = re.sub(r'([^\s])\.wasm', r'\1', s)
    # Same thing for .jsexe directories generated by the JS backend
    s = re.sub(r'([^\s])\.jsexe', r'\1', s)

    # hpc executable is given ghc suffix
    s = re.sub('hpc-ghc', 'hpc', s)

    # The inplace ghc's are called ghc-stage[123] to avoid filename
    # collisions, so we need to normalise that to just "ghc"
    s = re.sub('ghc-stage[123]', 'ghc', s)
    # Remove platform prefix (e.g. javascript-unknown-ghcjs) for cross-compiled tools
    # (ghc, ghc-pkg, unlit, etc.)
    s = re.sub(r'\w+(-\w+)*-ghc', 'ghc', s)
    s = re.sub(r'\w+(-\w+)*-unlit', 'unlit', s)

    # On windows error messages can mention versioned executables
    s = re.sub('ghc-[0-9.]+', 'ghc', s)
    s = re.sub('runghc-[0-9.]+', 'runghc', s)
    s = re.sub('hpc-[0-9.]+', 'hpc', s)
    s = re.sub('ghc-pkg-[0-9.]+', 'ghc-pkg', s)

    # Error messages sometimes contain ghc-bignum implementation package
    s = re.sub('ghc-bignum-[0-9.]+', 'ghc-bignum-<VERSION>', s)

    # Error messages sometimes contain these blurbs which can vary
    # spuriously depending upon build configuration (e.g. based on bignum
    # backend)
    s = re.sub('...plus ([a-z]+|[0-9]+) others',
                 '...plus N others', s)
    s = re.sub('...plus ([a-z]+|[0-9]+) instances involving out-of-scope types',
                 '...plus N instances involving out-of-scope types', s)

    # Also filter out bullet characters.  This is because bullets are used to
    # separate error sections, and tests shouldn't be sensitive to how the
    # the division happens.
    bullet = '•'.encode('utf8') if isinstance(s, bytes) else '•'
    s = s.replace(bullet, '')

    # Windows only, this is a bug in hsc2hs but it is preventing
    # stable output for the testsuite. See #9775. For now we filter out this
    # warning message to get clean output.
    if config.msys:
        s = re.sub('Failed to remove file (.*); error= (.*)$', '', s)
        s = re.sub(r'DeleteFile "(.+)": permission denied \(Access is denied\.\)(.*)$', '', s)

    # filter out unsupported GNU_PROPERTY_TYPE (5), which is emitted by LLVM10
    # and not understood by older binutils (ar, ranlib, ...)
    s = modify_lines(s, lambda l: re.sub(r'^(.+)warning: (.+): unsupported GNU_PROPERTY_TYPE \(5\) type: 0xc000000(.*)$', '', l))

    s = re.sub(r'ld: warning: passed .* min versions \(.*\) for platform macOS. Using [\.0-9]+.','',s)
    s = re.sub('ld: warning: -sdk_version and -platform_version are not compatible, ignoring -sdk_version','',s)
    # ignore superfluous dylibs passed to the linker.
    s = re.sub('ld: warning: .*, ignoring unexpected dylib file\n','',s)
    # ignore LLVM Version mismatch garbage; this will just break tests.
    s = re.sub('You are using an unsupported version of LLVM!.*\n','',s)
    s = re.sub('Currently only [\\.0-9]+ is supported. System LLVM version: [\\.0-9]+.*\n','',s)
    s = re.sub('We will try though\\.\\.\\..*\n','',s)
    # ignore warning about strip invalidating signatures
    s = re.sub('.*strip: changes being made to the file will invalidate the code signature in.*\n','',s)
    # clang may warn about unused argument when used as assembler
    s = re.sub('.*warning: argument unused during compilation:.*\n', '', s)
    # Emscripten displays cache info and old emcc doesn't support EMCC_LOGGING=0
    s = re.sub('cache:INFO: .*\n', '', s)

    return s

# normalise a .prof file, so that we can reasonably compare it against
# a sample.  This doesn't compare any of the actual profiling data,
# only the shape of the profile and the number of entries.
def normalise_prof (s: str) -> str:

    # sip everything up to the line beginning "COST CENTRE"
    s = re.sub('^(.*\n)*COST CENTRE[^\n]*\n','',s)

    # sip results for CAFs, these tend to change unpredictably
    s = re.sub('\n[ \t]*(CAF|IDLE).*\n','\n',s)

    # XXX Ignore Main.main.  Sometimes this appears under CAF, and
    # sometimes under MAIN.
    s = re.sub('[ \t]*main[ \t]+Main.*\n','\n',s)

    # The next step assumes none of the fields have no spaces in, which is broke
    # when the src = <no location info>
    s = re.sub('no location info','no-location-info', s)

    # Source locations from internal libraries, remove the source location
    # > libraries/ghc-internal/src/path/Foo.hs:204:1-18
    # => ghc-internal/src/path/Foo.hs
    s = re.sub(r'\slibraries/(\S+)(:\S+){2}\s', r' \1 ', s)

    # Source locations from internal libraries, remove the source location
    # > libraries/ghc-internal/src/path/Foo.hs::(2,1)-(5,38)
    # => ghc-internal/src/path/Foo.hs
    s = re.sub(r'\slibraries/(\S+)(:\S+){1}\s', r' \1 ', s)

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

    # Apply final normaliser, usually used to restrict profile to cost centres from
    # the module being tested to avoid being polluted with cost centres from base.
    final_norm = getTestOpts().extra_prof_normaliser
    s = final_norm(s)

    # Split 9 whitespace-separated groups, take columns 1 (cost-centre), 2
    # (module), 3 (src), and 5 (entries). SCC names can't have whitespace, so
    # this works fine.
    s = re.sub(r'\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*',
            '\\1 \\2 \\3 \\5\n', s)
    return s

def normalise_slashes_( s: str ) -> str:
    s = re.sub(r'\\', '/', s)
    s = re.sub(r'//', '/', s)
    return s

def normalise_exe_( s: str ) -> str:
    s = re.sub(r'\.exe', '', s)
    s = re.sub(r'\.wasm', '', s)
    s = re.sub(r'\.jsexe', '', s)
    return s

def normalise_output( s: str ) -> str:
    # remove " error:" and lower-case " Warning:" to make patch for
    # trac issue #10021 smaller
    s = modify_lines(s, lambda l: re.sub(' error:', '', l))
    s = modify_lines(s, lambda l: re.sub(' Warning:', ' warning:', l))
    # Remove a .exe extension (for Windows)
    # and .wasm extension (for the Wasm backend)
    # and .jsexe extension (for the JS backend)
    # This can occur in error messages generated by the program.
    s = re.sub(r'([^\s])\.exe', r'\1', s)
    s = re.sub(r'([^\s])\.wasm', r'\1', s)
    s = re.sub(r'([^\s])\.jsexe', r'\1', s)
    s = normalise_callstacks(s)
    s = normalise_type_reps(s)
    # ghci outputs are pretty unstable with -fexternal-dynamic-refs, which is
    # requires for -fPIC
    s = re.sub('  -fexternal-dynamic-refs\n','',s)
    s = re.sub(r'ld: warning: passed .* min versions \(.*\) for platform macOS. Using [\.0-9]+.','',s)
    s = re.sub('ld: warning: -sdk_version and -platform_version are not compatible, ignoring -sdk_version','',s)
    # ignore superfluous dylibs passed to the linker.
    s = re.sub('ld: warning: .*, ignoring unexpected dylib file\n','',s)
    # ignore LLVM Version mismatch garbage; this will just break tests.
    s = re.sub('You are using an unsupported version of LLVM!.*\n','',s)
    s = re.sub('Currently only [\\.0-9]+ is supported. System LLVM version: [\\.0-9]+.*\n','',s)
    s = re.sub('We will try though\\.\\.\\..*\n','',s)
    # ignore warning about strip invalidating signatures
    s = re.sub('.*strip: changes being made to the file will invalidate the code signature in.*\n','',s)
    # clang may warn about unused argument when used as assembler
    s = re.sub('.*warning: argument unused during compilation:.*\n', '', s)

    # strip the cross prefix if any
    s = re.sub(r'\w+(-\w+)*-ghc', 'ghc', s)

    return s

def normalise_asm( s: str ) -> str:
    lines = s.split('\n')
    # Only keep insuctions and labels not starting with a dot.
    metadata = re.compile('^[ \t]*\\..*$')
    out = []
    for line in lines:
      # Drop metadata directives (e.g. ".type")
      if not metadata.match(line):
        line = re.sub('@plt', '', line)
        ins = line.lstrip().split()
        # Drop empty lines.
        if not ins:
          continue
        # Drop operands, except for call insuctions.
        elif ins[0] == 'call':
          out.append(ins[0] + ' ' + ins[1])
        else:
          out.append(ins[0])
    return '\n'.join(out)

def safe_print(s: str) -> None:
    s2 = s.encode(sys.stdout.encoding, errors='replace').decode(sys.stdout.encoding)
    print(s2)

def if_verbose( n: int, s: str ) -> None:
    if config.verbose >= n:
        safe_print(s)

def dump_file(f: Path):
    try:
        with f.open() as file:
            safe_print(file.read())
    except Exception:
        print('')

# -----------------------------------------------------------------------------
# Run a program in the interpreter and check its output

async def runCmdPerf(
        perf_counters: List[str],
        cmd: str,
        output_file: str,
        working_dir: Optional[Path] = None,
        **kwargs)  -> int:
    """
    Run a command under `perf stat`, collecting the given counters.

    Returns the exit code and a dictionary of the collected counter values.

    If given a 'working_dir', we generate a command looking like:

    .. code-block:: text

        cd $working_dir && perf stat ... $cmd

    This allows users to find the test directory by looking at the execution logs,
    and allows us to write 'perf' output files in the test directory.
    """
    if len(perf_counters) == 0 or config.perf_path is None:
        if working_dir:
            cmd = f"cd \"{working_dir}\" && {cmd}"

        exit_code = await runCmd(cmd, **kwargs)
        return exit_code

    perf_cmd_args: List[str] = [str(config.perf_path), 'stat', '-x\\;', '-o', output_file, '-e', ','.join(perf_counters), cmd]
    cmd = ' '.join(perf_cmd_args)
    if working_dir:
        cmd = f"cd \"{working_dir}\" && {cmd}"

    exit_code = await runCmd(cmd, **kwargs)
    return exit_code

async def runCmd(cmd: str,
           stdin: Union[None, Path]=None,
           stdout: Union[None, Path]=None,
           stderr: Union[None, int, Path]=None,
           timeout_multiplier=1.0,
           print_output=False) -> int:
    """
    Run a command enforcing a timeout and returning the exit code.

    The process's working directory is changed to 'working_dir'.
    """

    timeout_prog = strip_quotes(config.timeout_prog)
    timeout = str(int(ceil(config.timeout * timeout_multiplier)))

    # Format cmd using config. Example: cmd='{hpc} report A.tix'
    cmd = cmd.format(**config.__dict__)
    if_verbose(3, '%s< %s' % (cmd, stdin.name if isinstance(stdin, Path) else ''))

    stdin_file = stdin.open('rb') if stdin is not None else None
    stdout_buffer = b''
    stderr_buffer = b''

    hStdErr = asyncio.subprocess.PIPE
    if stderr is asyncio.subprocess.STDOUT:
        hStdErr = asyncio.subprocess.STDOUT

    try:
        # cmd is a complex command in Bourne-shell syntax
        # e.g (cd . && 'C:/users/simonpj/HEAD/inplace/bin/ghc-stage2' ...etc)
        # Hence it must ultimately be run by a Bourne shell. It's timeout's job
        # to invoke the Bourne shell

        proc = await asyncio.create_subprocess_exec(timeout_prog, timeout, cmd,
                                                   stdin=stdin_file,
                                                   stdout=asyncio.subprocess.PIPE,
                                                   stderr=hStdErr,
                                                   env=ghc_env
                                                   )

        stdout_buffer, stderr_buffer = await proc.communicate()
    finally:
        if stdin_file:
            stdin_file.close()
        if config.verbose >= 1 and print_output:
            if stdout_buffer:
                sys.stdout.buffer.write(stdout_buffer)
            if stderr_buffer:
                sys.stderr.buffer.write(stderr_buffer)

        if stdout is not None:
            if isinstance(stdout, Path):
                stdout.write_bytes(stdout_buffer)
            else:
                with io.open(stdout, 'wb') as f:
                    f.write(stdout_buffer)
        if stderr is not None:
            if isinstance(stderr, Path):
                stderr.write_bytes(stderr_buffer)

    if proc.returncode == 98:
        # The python timeout program uses 98 to signal that ^C was pressed
        stopNow()
    if proc.returncode == 99 and getTestOpts().exit_code != 99:
        # Only print a message when timeout killed the process unexpectedly.
        if_verbose(1, 'Timeout happened...killed process "{0}"...\n'.format(cmd))
    return proc.returncode # type: ignore

# Each message should be kept lowercase
def exit_code_specific_message(exit_code: int) -> str:
    messages = {99: "test timeout"}
    return messages.get(exit_code, "")

def format_bad_exit_code_message(exit_code: int) -> str:
    ex_msg = exit_code_specific_message(exit_code)
    if ex_msg == "":
        return 'bad exit code (%d)' % exit_code
    else:
        return ': '.join(['bad exit code (%d)' % exit_code, ex_msg])

# -----------------------------------------------------------------------------
# checking if ghostscript is available for checking the output of hp2ps
def genGSCmd(psfile: Path) -> str:
    return '{{gs}} -dNODISPLAY -dBATCH -dQUIET -dNOPAUSE "{0}"'.format(psfile)

does_ghostscript_work_cache = None

async def does_ghostscript_work() -> bool:
    """
    Detect whether Ghostscript is functional.
    """
    global does_ghostscript_work_cache
    if does_ghostscript_work_cache is not None:
        return does_ghostscript_work_cache

    def gsNotWorking(reason: str) -> None:
        print("GhostScript not available for hp2ps tests:", reason)

    if config.gs is None:
        does_ghostscript_work_cache = False
        return False

    try:
        if await runCmd(genGSCmd(config.top / 'config' / 'good.ps')) != 0:
            gsNotWorking("gs can't process good input")
            does_ghostscript_work_cache = False
            return False
    except Exception as e:
        gsNotWorking('error invoking gs on bad input: %s' % e)
        does_ghostscript_work_cache = False
        return False

    try:
        cmd = genGSCmd(config.top / 'config' / 'bad.ps') + ' >/dev/null 2>&1'
        if await runCmd(cmd) == 0:
            gsNotWorking('gs accepts bad input')
            does_ghostscript_work_cache = False
            return False
    except Exception as e:
        gsNotWorking('error invoking gs on bad input: %s' % e)
        does_ghostscript_work_cache = False
        return False

    does_ghostscript_work_cache = True
    return True

def add_suffix( name: Union[str, Path], suffix: str ) -> Path:
    if suffix == '':
        return Path(name)
    else:
        return Path(str(name) + '.' + suffix)

def add_hs_lhs_suffix(name: str) -> Path:
    if getTestOpts().c_src:
        return add_suffix(name, 'c')
    elif getTestOpts().cmm_src:
        return add_suffix(name, 'cmm')
    elif getTestOpts().cxx_src:
        return add_suffix(name, 'cpp')
    elif getTestOpts().objc_src:
        return add_suffix(name, 'm')
    elif getTestOpts().objcxx_src:
        return add_suffix(name, 'mm')
    elif getTestOpts().literate:
        return add_suffix(name, 'lhs')
    else:
        return add_suffix(name, 'hs')

def in_testdir(name: Union[Path, str], suffix: str='') -> Path:
    return getTestOpts().testdir / add_suffix(name, suffix)

def in_srcdir(name: Union[Path, str], suffix: str='') -> Path:
    srcdir = getTestOpts().srcdir
    if srcdir is None:
        return add_suffix(name, suffix)
    else:
        return srcdir / add_suffix(name, suffix)

def in_statsdir(name: Union[Path, str], suffix: str='') -> Path:
    dir = config.stats_files_dir
    if dir is None:
        raise TypeError('stats_files_dir is not set')
    return dir / add_suffix(name, suffix)

# Finding the sample output.  The filename is of the form
#
#   <test>.stdout[-ws-<wordsize>][-<platform>|-<os>]
#
def find_expected_file(name: TestName, suff: str) -> Path:
    basename = add_suffix(name, suff)
    # Override the basename if the user has specified one, this will then be
    # subjected to the same name mangling scheme as normal to allow platform
    # specific overrides to work.
    basename = getTestOpts().use_specs.get(suff, basename)

    files = [str(basename) + ws + plat
             for plat in ['-' + config.platform, '-' + config.os, '']
             for ws in ['-ws-' + config.wordsize, '']]

    for f in files:
        if in_srcdir(f).exists():
            return f

    return basename

if config.msys:
    import stat
    def cleanup() -> None:
        testdir = getTestOpts().testdir_raw # type: Optional[Path]
        if testdir is None:
            return
        max_attempts = 5
        retries = max_attempts
        def on_error(function, path: str, excinfo):
            # At least one test (T11489) removes the write bit from a file it
            # produces. Windows refuses to delete read-only files with a
            # permission error. Try setting the write bit and try again.
            Path(path).chmod(stat.S_IWRITE)
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
        # See #13162
        exception = None
        while retries > 0 and testdir.exists():
            time.sleep((max_attempts-retries)*6)
            try:
                shutil.rmtree(str(testdir), onerror=on_error, ignore_errors=False)
            except Exception as e:
                exception = e
            retries -= 1

        if retries == 0 and testdir.exists():
            raise Exception("Unable to remove folder '%s': %s\nUnable to start current test."
                            % (testdir, exception))
else:
    def cleanup() -> None:
        testdir = getTestOpts().testdir_raw
        if testdir is not None and testdir.exists():
            shutil.rmtree(str(testdir), ignore_errors=False)


# -----------------------------------------------------------------------------
# Return a list of all the files ending in '.T' below directories roots.

def findTFiles(roots: List[str]) -> Iterator[str]:
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

def summary(t: TestRun, file: TextIO, color=False) -> None:

    file.write('\n')
    printUnexpectedTests(file,
        [t.unexpected_passes, t.unexpected_failures,
         t.unexpected_stat_failures, t.framework_failures])

    if len(t.unexpected_failures) > 0 or \
        len(t.unexpected_stat_failures) > 0 or \
        len(t.unexpected_passes) > 0 or \
        len(t.framework_failures) > 0:
        summary_color = Color.RED
    else:
        summary_color = Color.GREEN

    assert t.start_time is not None
    file.write(colored(summary_color, 'SUMMARY') + ' for test run started at '
               + t.start_time.strftime("%c %Z") + '\n'
               + str(datetime.datetime.now() - t.start_time).rjust(8)
               + ' spent to go through\n'
               + repr(t.total_tests).rjust(8)
               + ' total tests, which gave rise to\n'
               + repr(t.total_test_cases).rjust(8)
               + ' test cases, of which\n'
               + repr(t.n_tests_skipped).rjust(8)
               + ' were skipped\n'
               + repr(t.n_missing_libs).rjust(8)
               + ' had missing libraries\n'
               + '\n'
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
               + repr(len(t.fragile_failures) + len(t.fragile_passes)).rjust(8)
               + ' fragile tests\n'
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

def printUnexpectedTests(file: TextIO, testInfoss):
    unexpected = set(result.testname
                     for testInfos in testInfoss
                     for result in testInfos
                     if not result.testname.endswith('.T'))
    if unexpected:
        file.write('Unexpected results from:\n')
        file.write('TEST="' + ' '.join(sorted(unexpected)) + '"\n')
        file.write('\n')

def printTestInfosSummary(file: TextIO, testInfos):
    maxDirLen = max(len(tr.directory) for tr in testInfos)
    for result in sorted(testInfos, key=lambda r: (r.testname.lower(), r.way, r.directory)):
        directory = result.directory.ljust(maxDirLen)
        file.write('   {directory}  {r.testname} [{r.reason}] ({r.way})\n'.format(
            r = result,
            directory = directory))
    file.write('\n')

def modify_lines(s: str, f: Callable[[str], str]) -> str:
    s = '\n'.join([f(l) for l in s.splitlines()])
    if s and s[-1] != '\n':
        # Prevent '\ No newline at end of file' warnings when diffing.
        s += '\n'
    return s
