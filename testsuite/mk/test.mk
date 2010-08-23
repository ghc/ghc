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
#       THREADS         -- run n tests at once
#
# -----------------------------------------------------------------------------

# export the value of $MAKE for invocation in ghc-regress/driver/
export MAKE

RUNTESTS     = $(TOP)/driver/runtests.py
COMPILER     = ghc
CONFIGDIR    = $(TOP)/config
CONFIG       = $(CONFIGDIR)/$(COMPILER)

RUNTEST_OPTS =

$(eval $(call get-ghc-rts-field,WORDSIZE,Word size))
$(eval $(call get-ghc-rts-field,TARGETPLATFORM,Target platform))
$(eval $(call get-ghc-rts-field,TargetOS_CPP,Target OS))
$(eval $(call get-ghc-rts-field,TargetARCH_CPP,Target architecture))
ifeq "$(filter $(TargetOS_CPP), cygwin32 mingw32)" ""
exeext =
else
exeext = .exe
endif

$(eval $(call get-ghc-feature-bool,GhcWithNativeCodeGen,Have native code generator))
ifeq "$(GhcWithNativeCodeGen)" "YES"
RUNTEST_OPTS += -e ghc_with_native_codegen=1
else
RUNTEST_OPTS += -e ghc_with_native_codegen=0
endif

HASKELL98_LIBDIR := $(shell "$(GHC_PKG)" field haskell98 library-dirs | sed 's/^[^:]*: *//')
HAVE_PROFILING := $(shell if [ -f $(HASKELL98_LIBDIR)/libHShaskell98-*_p.a ]; then echo YES; else echo NO; fi)

ifeq "$(HAVE_PROFILING)" "YES"
RUNTEST_OPTS += -e ghc_with_profiling=1
else
RUNTEST_OPTS += -e ghc_with_profiling=0
endif

ifeq "$(filter thr, $(GhcRTSWays))" "thr"
RUNTEST_OPTS += -e ghc_with_threaded_rts=1
else
RUNTEST_OPTS += -e ghc_with_threaded_rts=0
endif

ifeq "$(filter dyn, $(GhcRTSWays))" "dyn"
RUNTEST_OPTS += -e ghc_with_dynamic_rts=1
else
RUNTEST_OPTS += -e ghc_with_dynamic_rts=0
endif

$(eval $(call get-ghc-feature-bool,GhcWithInterpreter,Have interpreter))
ifeq "$(GhcWithInterpreter)" "YES"
RUNTEST_OPTS += -e ghc_with_interpreter=1
else
RUNTEST_OPTS += -e ghc_with_interpreter=0
endif

$(eval $(call get-ghc-feature-bool,GhcUnregisterised,Unregisterised))
ifeq "$(GhcUnregisterised)" "YES"
RUNTEST_OPTS += -e ghc_unregisterised=1
else
RUNTEST_OPTS += -e ghc_unregisterised=0
endif

$(eval $(call get-ghc-feature-bool,GhcWithSMP,Support SMP))
ifeq "$(GhcWithSMP)" "YES"
RUNTEST_OPTS += -e ghc_with_smp=1
else
RUNTEST_OPTS += -e ghc_with_smp=0
endif

ifneq "$(shell llvmc --version | grep version)" ""
RUNTEST_OPTS += -e ghc_with_llvm=1
else
RUNTEST_OPTS += -e ghc_with_llvm=0
endif

ifeq "$(WINDOWS)" "YES"
RUNTEST_OPTS += -e windows=True
else
RUNTEST_OPTS += -e windows=False
endif

ifeq "$(DARWIN)" "YES"
RUNTEST_OPTS += -e darwin=True
else
RUNTEST_OPTS += -e darwin=False
endif

ifeq "$(IN_TREE_COMPILER)" "YES"
RUNTEST_OPTS += -e in_tree_compiler=True
else
RUNTEST_OPTS += -e in_tree_compiler=False
endif

ifneq "$(THREADS)" ""
RUNTEST_OPTS += --threads=$(THREADS)
endif

RUNTEST_OPTS +=  \
	--rootdir=. \
	--config=$(CONFIG) \
	-e 'config.confdir="$(CONFIGDIR)"' \
	-e 'config.compiler="$(TEST_HC)"' \
	-e 'config.compiler_always_flags.append("$(EXTRA_HC_OPTS)")' \
	-e 'config.ghc_pkg="$(GHC_PKG)"' \
	-e 'config.hp2ps="$(HP2PS_ABS)"' \
	-e 'config.hpc="$(HPC)"' \
	-e 'config.gs="$(GS)"' \
	-e 'config.platform="$(TARGETPLATFORM)"' \
	-e 'config.os="$(TargetOS_CPP)"' \
	-e 'config.arch="$(TargetARCH_CPP)"' \
	-e 'config.wordsize="$(WORDSIZE)"' \
	-e 'default_testopts.cleanup="$(CLEANUP)"' \
	-e 'config.timeout=int($(TIMEOUT)) or config.timeout' \
	-e 'config.timeout_prog="$(TIMEOUT_PROGRAM)"' \
	-e 'config.exeext="$(exeext)"' \
	-e 'config.top="$(TOP_ABS)"' \
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

.PHONY: all boot test verbose accept fast

all: test

TIMEOUT_PROGRAM = $(TOP)/timeout/install-inplace/bin/timeout$(exeext)

boot: $(TIMEOUT_PROGRAM)

$(TIMEOUT_PROGRAM) :
	@echo "Looks like you don't have timeout, building it first..."
	$(MAKE) -C $(TOP)/timeout all

test: $(TIMEOUT_PROGRAM)
	$(PYTHON) $(RUNTESTS) $(RUNTEST_OPTS) \
		$(patsubst %, --only=%, $(TEST)) \
		$(patsubst %, --only=%, $(TESTS)) \
		$(patsubst %, --way=%, $(WAY)) \
		$(patsubst %, --skipway=%, $(SKIPWAY)) \
		$(setfast) \
		$(setaccept)

verbose: test

accept:
	$(MAKE) accept=YES

fast:
	$(MAKE) fast=YES

