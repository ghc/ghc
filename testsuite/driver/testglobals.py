#
# (c) Simon Marlow 2002
#

from my_typing import *
from pathlib import Path
from perf_notes import MetricChange, PerfStat, Baseline, GitRef
from datetime import datetime

# -----------------------------------------------------------------------------
# Configuration info

# There is a single global instance of this structure, stored in the
# variable config below.  The fields of the structure are filled in by
# the appropriate config script(s) for this compiler/platform, in
# ../config.
#
# Bits of the structure may also be filled in from the command line,
# via the build system, using the '-e' option to runtests.

class TestConfig:
    def __init__(self):

        # Where the testsuite root is
        self.top = Path('.')

        # Directories below which to look for test description files (foo.T)
        self.rootdirs = []

        # Run these tests only (run all tests if empty)
        self.run_only_some_tests = False
        self.only = set()

        # Don't fail on out-of-tolerance stat failures
        self.ignore_perf_increases = False
        self.ignore_perf_decreases = False

        # Accept new output which differs from the sample?
        self.accept = False
        self.accept_platform = False
        self.accept_os = False

        # File in which to save the performance metrics.
        self.metrics_file = ''

        # File in which to save the summary
        self.summary_file = ''

        # Path to Ghostscript
        self.gs = None # type: Optional[Path]

        # Run tests requiring Haddock
        self.haddock = False

        # Compiler has native code generator?
        self.have_ncg = False

        # Is compiler unregisterised?
        self.unregisterised = False

        # Was the compiler executable compiled with profiling?
        self.compiler_profiled = False

        # Was the compiler compiled with DEBUG?
        self.compiler_debugged = False

        # Was the compiler compiled with -debug?
        self.debug_rts = False

        # Was the compiler compiled with LLVM?
        self.ghc_built_by_llvm = False

        # What platform are we running on?
        self.platform = ''
        self.os = ''
        self.arch = ''
        self.msys = False
        self.cygwin = False

        # What is the wordsize (in bits) of this platform?
        self.wordsize = ''

        # Is tables next to code enabled?
        self.tables_next_to_code = True
        # Verbosity level
        self.verbose = 2

        # See Note [validate and testsuite speed] in `validate`
        self.speed = 1

        self.list_broken = False

        # Path to the compiler (stage2 by default)
        self.compiler = ''
        # and ghc-pkg
        self.ghc_pkg = ''

        # Is self.compiler a stage 1, 2 or 3 compiler?
        self.stage = 2

        # Flags we always give to this compiler
        self.compiler_always_flags = []

        # Which ways to run tests (when compiling and running respectively)
        # Other ways are added from the command line if we have the appropriate
        # libraries.
        self.compile_ways = [] # type: List[WayName]
        self.run_ways     = [] # type: List[WayName]
        self.other_ways   = [] # type: List[WayName]

        # The ways selected via the command line.
        self.cmdline_ways = [] # type: List[WayName]

        # Lists of flags for each way
        self.way_flags = {}  # type: Dict[WayName, List[str]]
        self.way_rts_flags = {}  # type: Dict[WayName, List[str]]

        # Do we have a functional LLVM toolchain?
        self.have_llvm = False

        # Do we have vanilla libraries?
        self.have_vanilla = False

        # Do we have dynamic libraries?
        self.have_dynamic = False

        # Do we have profiling support?
        self.have_profiling = False

        # Do we have interpreter support?
        self.have_interp = False

        # Do we have RTS linker?
        self.have_RTS_linker = False

        # Do we have threaded RTS?
        self.ghc_with_threaded_rts = False

        # Do we even have processes?
        self.have_process = True

        # Does the platform support loading of dynamic shared libraries? e.g.
        # some musl-based environments do not.
        self.supports_dynamic_libs = True

        # Does GHC support dynamic linking of Haskell objects (i.e. the dynamic
        # way)?
        self.supports_dynamic_hs = True

        # Is the compiler dynamically linked?
        self.ghc_dynamic = False

        # Does the host RTS have SMP support?
        self.ghc_has_smp = True

        # Does the target have SMP support?
        self.target_has_smp = False

        # Is gdb available?
        self.have_gdb = False

        # Is readelf available?
        self.have_readelf = False

        # Do we use a fast backend for bignum (e.g. GMP)
        self.have_fast_bignum = True

        # Are we testing an in-tree compiler?
        self.in_tree_compiler = True

        # Are we running in a ThreadSanitizer-instrumented build?
        self.have_thread_sanitizer = False

        # Do symbols use leading underscores?
        self.leading_underscore = False

        # the timeout program
        self.timeout_prog = ''
        self.timeout = 300

        # threads
        self.threads = 1

        # An optional executable used to wrap target code execution
        self.target_wrapper = None

        # tests which should be considered to be broken during this testsuite
        # run.
        self.broken_tests = set() # type: Set[TestName]

        # Baseline commit for performance metric comparisons.
        self.baseline_commit = None # type: Optional[GitRef]

        # Should we skip performance tests
        self.skip_perf_tests = False

        # Only do performance tests
        self.only_perf_tests = False

        # Allowed performance changes (see perf_notes.get_allowed_perf_changes())
        self.allowed_perf_changes = {}

        # The test environment.
        self.test_env = 'local'

        # terminal supports colors
        self.supports_colors = False

        # Where to look up runtime stats produced by haddock, needed for
        # the haddock perf tests in testsuite/tests/perf/haddock/.
        # See Note [Haddock runtime stats files] at the bottom of this file.
        self.stats_files_dir = Path('/please_set_stats_files_dir')

        # Where to place observed output files on when unexpected output
        # is observed.
        self.unexpected_output_dir = None # type: Optional[Path]

        # Should we cleanup after test runs?
        self.cleanup = True

        # I have no idea what this does
        self.package_conf_cache_file = None # type: Optional[Path]

        # the libdir for the test compiler. Set by hadrian, see
        # Setting.Builders.RunTest
        self.libdir = ''

        # The extra hadrian dependencies we need for all configured tests
        self.hadrian_deps = set() # type: Set[str]

        # Are we only reporting hadrian dependencies?
        # The path specifies the file in which to write the dependencies
        self.only_report_hadrian_deps = None # type: Optional[Path]

    def validate(self) -> None:
        """ Check the TestConfig for self-consistency """
        def assert_implies(a: bool, b: bool):
            if a:
                assert(b)

        assert_implies(self.supports_dynamic_hs, self.supports_dynamic_libs)
        assert_implies(self.have_dynamic, self.supports_dynamic_hs)
        assert_implies(self.ghc_dynamic, self.have_dynamic)

global config
config = TestConfig()

def getConfig():
    return config

import os
# This is the environment in which we run test subprocesses.
global ghc_env
ghc_env = os.environ.copy()

# -----------------------------------------------------------------------------
# Information about the current test run

class TestResult:
    """
    A result from the execution of a test. These live in the expected_passes,
    framework_failures, framework_warnings, unexpected_passes,
    unexpected_failures, unexpected_stat_failures lists of TestRun.
    """
    __slots__ = 'directory', 'testname', 'reason', 'way', 'stdout', 'stderr'
    def __init__(self,
                 directory: str,
                 testname: TestName,
                 reason: str,
                 way: WayName,
                 stdout: Optional[str]=None,
                 stderr: Optional[str]=None) -> None:
        self.directory = directory
        self.testname = testname
        self.reason = reason
        self.way = way
        self.stdout = stdout
        self.stderr = stderr

# A performance metric measured in this test run.
PerfMetric = NamedTuple('PerfMetric',
                        [('change', MetricChange),
                         ('stat', PerfStat),
                         ('baseline', Optional[Baseline]) ])

class TestRun:
   def __init__(self) -> None:
       self.start_time = None # type: Optional[datetime]
       self.total_tests = 0
       self.total_test_cases = 0

       self.n_tests_skipped = 0
       self.n_missing_libs = 0
       self.n_expected_passes = 0
       self.n_expected_failures = 0

       self.framework_failures = [] # type: List[TestResult]
       self.framework_warnings = [] # type: List[TestResult]

       self.expected_passes = [] # type: List[TestResult]
       self.unexpected_passes = [] # type: List[TestResult]
       self.unexpected_failures = [] # type: List[TestResult]
       self.unexpected_stat_failures = [] # type: List[TestResult]

       # Results from tests that have been marked as fragile
       self.fragile_passes = [] # type: List[TestResult]
       self.fragile_failures = [] # type: List[TestResult]

       # List of all metrics measured in this test run.
       # [(change, PerfStat)] where change is one of the  MetricChange
       # constants: NewMetric, NoChange, Increase, Decrease.
       # NewMetric happens when the previous git commit has no metric recorded.
       self.metrics = [] # type: List[PerfMetric]

global t
t = TestRun()

# -----------------------------------------------------------------------------
# Information about the current test

class TestOptions:
   def __init__(self) -> None:
       # skip this test?
       self.skip = False

       # the test is known to be fragile in these ways
       self.fragile_ways = [] # type: List[WayName]

       # skip these ways
       self.omit_ways = [] # type: List[WayName]

       # skip all ways except these (None == do all ways)
       self.only_ways = None # type: Optional[List[WayName]]

       # add these ways to the default set
       self.extra_ways = [] # type: List[WayName]

       # the result we normally expect for this test
       self.expect = 'pass'

       # override the expected result for certain ways
       self.expect_fail_for = [] # type: List[WayName]

       # the stdin file that this test will use (None for <name>.stdin)
       self.srcdir = None # type: Optional[Path]

       # the stdin file that this test will use (None for <name>.stdin)
       self.stdin = None # type: Optional[Path]

       # Set the expected stderr/stdout. '' means infer from test name.
       self.use_specs = {} # type: Dict[str, Path]

       # don't compare output
       self.ignore_stdout = False
       self.ignore_stderr = False

       # don't use the executable extension
       self.ignore_extension = False

       # Backpack test
       self.compile_backpack = False

       # We sometimes want to modify the compiler_always_flags, so
       # they are copied from config.compiler_always_flags when we
       # make a new instance of TestOptions.
       self.compiler_always_flags = [] # type: List[str]

       # extra compiler opts for this test
       self.extra_hc_opts = ''

       # extra run opts for this test
       self.extra_run_opts = ''

       # expected exit code
       self.exit_code = 0 # type: int

       # extra files to clean afterward
       self.clean_files = [] # type: List[str]

       # extra files to copy to the testdir
       self.extra_files = [] # type: List[str]

       # Is the test testing performance?
       self.is_stats_test = False

       # Does this test the compiler's performance as opposed to the generated code.
       self.is_compiler_stats_test = False

       # Map from metric to information about that metric
       #    { metric: { "deviation": <int>
       #                The action to run to get the current value of the test
       #              , "current": lambda way: <Int>
       #                The action to run to get the baseline value of the test
       #              , "baseline": lambda way, commit: baseline value } }
       self.generic_stats_test: Dict  = {} # Dict

       # should we run this test alone, i.e. not run it in parallel with
       # any other threads
       self.alone = False

       # Does this test use a literate (.lhs) file?
       self.literate = False

       # Does this test use a .c, .cpp, .m or .mm file?
       self.c_src      = False
       self.cxx_src    = False
       self.objc_src   = False
       self.objcxx_src = False

       # Does this test use a .cmm file?
       self.cmm_src    = False

       # Should we put .hi/.o files in a subdirectory?
       self.outputdir = None

       # Command to run before the test
       self.pre_cmd = None

       # Optional per test target wrapper that overrides the default
       # one when testing a cross target
       self.target_wrapper = None

       # Command wrapper: a function to apply to the command before running it
       self.cmd_wrapper = None

       # Prefix to put on the command before compiling it
       self.compile_cmd_prefix = ''

       # Extra output normalisation
       self.extra_normaliser = lambda x: x # type: OutputNormalizer

       # Custom output checker, otherwise do a comparison with expected
       # stdout file.  Accepts two arguments: filename of actual stdout
       # output, and a normaliser function given other test options
       self.check_stdout = None # type: Optional[Callable[[Path, OutputNormalizer], bool]]

       # Check .hp file when profiling libraries are available?
       self.check_hp = True

       # Extra normalisation for compiler error messages
       self.extra_errmsg_normaliser = lambda x: x

       # Keep profiling callstacks.
       self.keep_prof_callstacks = False

       # The directory the test is run.
       # Usually you should check the self.testdir property instead,
       # which crashes when this is not initialised.
       self.testdir_raw = None # type: Optional[Path]

       # Should we redirect stdout and stderr to a single file?
       self.combined_output = False

       # How should the timeout be adjusted on this test?
       self.pre_cmd_timeout_multiplier = 1.0
       self.compile_timeout_multiplier = 1.0
       self.run_timeout_multiplier = 1.0

       # Should we run tests in a local subdirectory (<testname>-run) or
       # in temporary directory in /tmp? See Note [Running tests in /tmp].
       self.local = True

       # Should we copy the files of symlink the files for the test?
       self.copy_files = False

       # The extra hadrian dependencies we need for this particular test
       self.hadrian_deps = set(["test:ghc"]) # type: Set[str]

   @property
   def testdir(self) -> Path:
       if self.testdir_raw is None:
           raise Exception('testdir not initialized')
       return self.testdir_raw

# The default set of options
global default_testopts
default_testopts = TestOptions()

# (bug, directory, name) of tests marked broken. Used by config.list_broken
# feature.
global brokens
brokens = []  # type: List[Tuple[IssueNumber, str, str]]
