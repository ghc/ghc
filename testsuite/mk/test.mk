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

# ghastly hack, because the driver requires that $tool be an absolute path name.
GHC_INPLACE_ABS	= $(FPTOOLS_TOP_ABS)/ghc/compiler/ghc-inplace

RUNTESTS     = $(TOP)/driver/runtests
RUNTEST_OPTS =  --config=$(CONFIG) tool=$(GHC_INPLACE_ABS) extra_hc_flags="$(EXTRA_HC_OPTS)" $(EXTRA_RUNTEST_OPTS)
CONFIG       = $(TOP)/config/msrc/cam-02-unx.T

TESTS	     = 
TEST	     = 

all :: test

test:
	$(RUNTESTS) $(RUNTEST_OPTS) $(TEST) $(TESTS)

verbose:
	$(RUNTESTS) $(RUNTEST_OPTS) verbose= $(TEST) $(TESTS)

accept:
	$(RUNTESTS) $(RUNTEST_OPTS) accept= verbose= $(TEST) $(TESTS)

