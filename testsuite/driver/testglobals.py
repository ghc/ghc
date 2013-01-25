# 
# (c) Simon Marlow 2002
#

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
        self.top = ''

        # Directories below which to look for test description files (foo.T)
        self.rootdirs = []

        # Run these tests only (run all tests if empty)
        self.only = []

        # Accept new output which differs from the sample?
        self.accept = 0

        # File in which to save the summary
        self.output_summary = ''

        # File in which to save the times
        self.times_file = ''

        # What platform are we running on?
        self.platform = ''
        self.os = ''
        self.arch = ''

        # What is the wordsize (in bits) of this platform?
        self.wordsize = ''

        # Verbosity level
        self.verbose = 1

        # run the "fast" version of the test suite
        self.fast = 0

        # Compiler type (ghc, hugs, nhc, etc.)
        self.compiler_type = ''

        # Path to the compiler
        self.compiler = ''
        # and ghc-pkg
        self.ghc_pkg = ''

        # Compiler version info
        self.compiler_version = ''
        self.compiler_maj_version = ''
        self.compiler_tags = []

        # Flags we always give to this compiler
        self.compiler_always_flags = []
        
        # Which ways to run tests (when compiling and running respectively)
        # Other ways are added from the command line if we have the appropriate
        # libraries.
        self.compile_ways = []
        self.run_ways     = []
        self.other_ways   = []

        # The ways selected via the command line.
        self.cmdline_ways = []

        # Lists of flags for each way
        self.way_flags = {}
        self.way_rts_flags = {}

        # Do we have vanilla libraries?
        self.have_vanilla = False

        # Do we have dynamic libraries?
        self.have_dynamic = False

        # Do we have profiling support?
        self.have_profiling = False

        # Do we have interpreter support?
        self.have_interp = False

        # Do we have shared libraries?
        self.have_shared_libs = False

        # Do we have SMP support?
        self.have_smp = False

        # Are we testing an in-tree compiler?
        self.in_tree_compiler = True

        # the timeout program
        self.timeout_prog = ''
        self.timeout = 300
        
        # threads
        self.threads = 1
        self.use_threads = 0

        # Should we check for files being written more than once?
        self.check_files_written = False

global config
config = TestConfig()

def getConfig():
    return config

# -----------------------------------------------------------------------------
# Information about the current test run

class TestRun:
   def __init__(self):
       self.start_time = ''
       self.total_tests = 0
       self.total_test_cases = 0
       self.n_framework_failures = 0
       self.framework_failures = {}
       self.n_tests_skipped = 0
       self.tests_skipped = {}
       self.n_expected_passes = 0
       self.expected_passes = {}
       self.n_expected_failures = 0
       self.expected_failures = {}
       self.n_missing_libs = 0
       self.missing_libs = {}
       self.n_unexpected_passes = 0
       self.unexpected_passes = {}
       self.n_unexpected_failures = 0
       self.unexpected_failures = {}
       
global t
t = TestRun()

def getTestRun():
    return t

# -----------------------------------------------------------------------------
# Information about the current test

class TestOptions:
   def __init__(self):
       # if not None then we look for namebase.stderr etc rather than
       # using the test name
       self.with_namebase = None

       # skip this test?
       self.skip = 0

       # skip these ways
       self.omit_ways = []

       # skip all ways except these (None == do all ways)
       self.only_ways = None

       # add these ways to the default set
       self.extra_ways = []

       # the result we normally expect for this test
       self.expect = 'pass'

       # override the expected result for certain ways
       self.expect_fail_for = []

       # the stdin file that this test will use (empty for <name>.stdin)
       self.stdin = ''

       # don't compare output
       self.ignore_output = 0

       # don't give anything as stdin
       self.no_stdin = 0

       # compile this test to .hc only
       self.compile_to_hc = 0

       # We sometimes want to modify the compiler_always_flags, so
       # they are copied from config.compiler_always_flags when we
       # make a new instance of TestOptions.
       self.compiler_always_flags = []

       # extra compiler opts for this test
       self.extra_hc_opts = ''

       # extra run opts for this test
       self.extra_run_opts = ''

       # expected exit code
       self.exit_code = 0

       # should we clean up after ourselves?
       self.cleanup = ''

       # extra files to clean afterward
       self.clean_files = []

       # which -t numeric fields do we want to look at, and what bounds must
       # they fall within?
       # Elements of these lists should be things like
       # ('bytes allocated',
       #   9300000000,
       #   10)
       # To allow a 10% deviation from 9300000000.
       self.compiler_stats_range_fields = {}
       self.stats_range_fields = {}

       # TODO: deprecate this in favour of compiler_stats_range_fields
       #
       # which -t numeric fields do we want to look at, and what bounds must
       # they fall within?
       # Elements of these lists should be things like
       # ('bytes allocated',
       #   9300000000,
       #   9400000000)
       self.compiler_stats_num_fields = {}
       self.stats_num_fields = {}

       # should we run this test alone, i.e. not run it in parallel with
       # any other threads
       self.alone = False

       # Does this test use a literate (.lhs) file?
       self.literate = 0

       # Does this test use a .c, .m or .mm file?
       self.c_src      = 0
       self.objc_src   = 0
       self.objcpp_src = 0

       # Does this test use a .cmm file?
       self.cmm_src    = 0

       # Should we put .hi/.o files in a subdirectory?
       self.outputdir = None

       # Command to run before the test
       self.pre_cmd = None

       # Command to run for extra cleaning
       self.clean_cmd = None

       # Command wrapper: a function to apply to the command before running it
       self.cmd_wrapper = None

       # Prefix to put on the command before compiling it
       self.compile_cmd_prefix = ''

       # Extra output normalisation
       self.extra_normaliser = lambda x: x

       # Extra normalisation for compiler error messages
       self.extra_errmsg_normaliser = lambda x: x

       # The directory the test is in
       self.testdir = '.'

       # Should we redirect stdout and stderr to a single file?
       self.combined_output = False

       # How should the timeout be adjusted on this test?
       self.timeout_multiplier = 1.0

# The default set of options
global default_testopts
default_testopts = TestOptions()

