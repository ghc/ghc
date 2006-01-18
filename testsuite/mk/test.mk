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
#	COMPILER	-- select a configuration file from config/
#
# -----------------------------------------------------------------------------

include $(TOP)/mk/wordsize.mk

ifeq "$(PYTHON)" ""
$(error Python must be installed in order to use the testsuite)
endif

# export the value of $MAKE for invocation in ghc-regress/driver/
export MAKE

# ghastly hack, because the driver requires that $tool be an absolute path name.
GHC_INPLACE_ABS	= $(FPTOOLS_TOP_ABS)/ghc/compiler/ghc-inplace

EXTRA_HC_OPTS += -D$(HostPlatform_CPP)
  # ideally TargetPlatform_CPP, but that doesn't exist; they're always the same anyway
RUNTESTS     = $(TOP)/driver/runtests.py
COMPILER     = ghc
CONFIG       = $(TOP)/config/$(COMPILER)

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

ifeq "$(filter u, $(GhcLibWays))" "u"
RUNTEST_OPTS += -e ghc_with_unreg=1
else
RUNTEST_OPTS += -e ghc_with_unreg=0
endif

ifeq "$(filter s, $(GhcLibWays))" "s"
RUNTEST_OPTS += -e ghc_with_smp=1
else
RUNTEST_OPTS += -e ghc_with_smp=0
endif

ifeq "$(GhcWithInterpreter)" "YES"
RUNTEST_OPTS += -e ghc_with_interpreter=1
else
RUNTEST_OPTS += -e ghc_with_interpreter=0
endif

ifeq "$(filter thr, $(GhcRTSWays))" "thr"
RUNTEST_OPTS += -e ghc_with_threaded_rts=1
else
RUNTEST_OPTS += -e ghc_with_threaded_rts=0
endif

RUNTEST_OPTS +=  \
	--config=$(CONFIG) \
	-e config.compiler=\"$(TEST_HC)\" \
	-e config.compiler_always_flags.append"(\"$(EXTRA_HC_OPTS)\")" \
	-e config.platform=\"$(TARGETPLATFORM)\" \
	-e config.wordsize=\"$(WORDSIZE)\" \
	-e default_testopts.cleanup=\"$(CLEANUP)\" \
	-e config.timeout_prog=\"$(TOP)/timeout/timeout\" \
	$(EXTRA_RUNTEST_OPTS)

ifeq "$(fast)" "YES"
setfast = -e config.fast=1
else
setfast = 
endif

ifeq "$(accept)" "YES"
setaccept = -e config.accept=1
else
setaccept = 
endif

TESTS	     = 
TEST	     = 
WAY =

all :: test

test:
	$(PYTHON) $(RUNTESTS) $(RUNTEST_OPTS) \
		$(patsubst %, --only=%, $(TEST)) \
		$(patsubst %, --only=%, $(TESTS)) \
		$(patsubst %, --way=%, $(WAY)) \
		$(setfast)
		$(setaccept)

verbose: test

accept:
	$(MAKE) accept=YES

fast:
	$(MAKE) fast=YES

