# -----------------------------------------------------------------------------
# Examples of use:
#
# 	make		-- run all the tests in the current directory
# 	make verbose	-- as make test, but up the verbosity
# 	make accept	-- run the tests, accepting the current output
#
# The following variables may be set on the make command line:
#
#	TEST		-- specific test to run
#	TESTS		-- specific tests to run (same as $TEST really)
#	EXTRA_HC_OPTS	-- extra flags to send to the Haskell compiler
#	EXTRA_RUNTEST_OPTS -- extra flags to give the test driver
#	CONFIG		-- use a different configuration file
#
# -----------------------------------------------------------------------------

ifeq "$(PYTHON)" ""
$(error Python must be installed in order to use the testsuite)
endif

# ghastly hack, because the driver requires that $tool be an absolute path name.
GHC_INPLACE_ABS	= $(FPTOOLS_TOP_ABS)/ghc/compiler/ghc-inplace

EXTRA_HC_OPTS += -D$(HostPlatform_CPP)
  # ideally TargetPlatform_CPP, but that doesn't exist; they're always the same anyway
RUNTESTS     = $(TOP)/driver/runtests.py
CONFIG       = $(TOP)/config/ghc

# can be overriden from the command line
TEST_HC = $(GHC_INPLACE_ABS)

RUNTEST_OPTS =

ifeq "$(GhcWithNativeCodeGen)" "YES"
RUNTEST_OPTS += -e ghc_with_native_codegen=1
else
RUNTEST_OPTS += -e ghc_with_native_codegen=0
endif

ifeq "$(filter p, $(GhcLibWays))" "p"
RUNTEST_OPTS += -e ghc_with_profiling=1
else
RUNTEST_OPTS += -e ghc_with_profiling=0
endif

RUNTEST_OPTS +=  \
	--config=$(CONFIG) \
	-e config.compiler=\"$(TEST_HC)\" \
	-e config.compiler_always_flags.append"(\"$(EXTRA_HC_OPTS)\")" \
	-e config.platform=\"$(TARGETPLATFORM)\" \
	$(EXTRA_RUNTEST_OPTS)

TESTS	     = 
TEST	     = 
WAY          =

all :: test

test:
	$(PYTHON) $(RUNTESTS) $(RUNTEST_OPTS) \
		$(patsubst %, --only=%, $(TEST)) \
		$(patsubst %, --only=%, $(TESTS)) \
		$(patsubst %, --way=%, $(WAY))

verbose: test

accept:
	$(PYTHON) $(RUNTESTS) $(RUNTEST_OPTS) \
		$(patsubst %, --only=%, $(TEST)) \
		$(patsubst %, --only=%, $(TESTS)) \
		$(patsubst %, --way=%, $(WAY)) \
		-e config.accept=1

