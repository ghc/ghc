# -----------------------------------------------------------------------------
# Examples of use:
#
# 	make		-- run all the tests in the current directory
# 	make verbose	-- as make test, but up the verbosity
# 	make accept	-- run the tests, accepting the current output
#
# The following variables may be set on the make command line:
#
#	TESTS		-- specific tests to run
#	EXTRA_HC_OPTS	-- extra flags to send to the Haskell compiler
#	EXTRA_RUNTEST_OPTS -- extra flags to give the test driver
#	CONFIG		-- use a different configuration file
#
# -----------------------------------------------------------------------------

RUNTESTS     = $(TOP)/driver/runtests
RUNTEST_OPTS =  --config=$(CONFIG) tool=$(GHC_INPLACE) extra_hc_flags="$(EXTRA_HC_OPTS)" $(EXTRA_RUNTEST_OPTS)
CONFIG       = $(TOP)/config/msrc/cam-02-unx.T
TESTS	     = 

all :: test

test:
	$(RUNTESTS) $(RUNTEST_OPTS) $(TESTS)

verbose:
	$(RUNTESTS) $(RUNTEST_OPTS) verbose= $(TESTS)

accept:
	$(RUNTESTS) $(RUNTEST_OPTS) accept= verbose= $(TESTS)

include $(FPTOOLS_TOP)/mk/target.mk
