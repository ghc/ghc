# -----------------------------------------------------------------------------
# Examples of use:
#
#  make           -- run all the tests in the current directory
#  make verbose   -- as make test, but up the verbosity
#  make accept    -- run the tests, accepting the current output
#
# The following variables may be set on the make command line:
#
#  TEST      -- specific test to run
#  TESTS     -- specific tests to run (same as $TEST really)
#  EXTRA_HC_OPTS      -- extra flags to send to the Haskell compiler
#  EXTRA_RUNTEST_OPTS -- extra flags to give the test driver
#  CONFIG    -- use a different configuration file
#  COMPILER  -- select a configuration file from config/
#  THREADS   -- run n tests at once
#
# -----------------------------------------------------------------------------

# export the value of $MAKE for invocation in tests/driver/
export MAKE

RUNTESTS     = $(TOP)/driver/runtests.py
COMPILER     = ghc
CONFIGDIR    = $(TOP)/config
CONFIG       = $(CONFIGDIR)/$(COMPILER)

# TEST_HC_OPTS is passed to every invocation of TEST_HC 
# in nested Makefiles
TEST_HC_OPTS = -fforce-recomp -dcore-lint -dcmm-lint -dno-debug-output -no-user-$(GhcPackageDbFlag) -rtsopts $(EXTRA_HC_OPTS)

RUNTEST_OPTS =

ifeq "$(filter $(TargetOS_CPP), cygwin32 mingw32)" ""
exeext =
else
exeext = .exe
endif

RUNTEST_OPTS += -e ghc_compiler_always_flags="'$(TEST_HC_OPTS)'"

RUNTEST_OPTS += -e ghc_debugged=$(GhcDebugged)

ifeq "$(GhcWithNativeCodeGen)" "YES"
RUNTEST_OPTS += -e ghc_with_native_codegen=1
else
RUNTEST_OPTS += -e ghc_with_native_codegen=0
endif

GHC_PRIM_LIBDIR := $(shell "$(GHC_PKG)" field ghc-prim library-dirs --simple-output)
HAVE_VANILLA := $(shell if [ -f $(subst \,/,$(GHC_PRIM_LIBDIR))/GHC/PrimopWrappers.hi ]; then echo YES; else echo NO; fi)
HAVE_DYNAMIC := $(shell if [ -f $(subst \,/,$(GHC_PRIM_LIBDIR))/GHC/PrimopWrappers.dyn_hi ]; then echo YES; else echo NO; fi)
HAVE_PROFILING := $(shell if [ -f $(subst \,/,$(GHC_PRIM_LIBDIR))/GHC/PrimopWrappers.p_hi ]; then echo YES; else echo NO; fi)

ifeq "$(HAVE_VANILLA)" "YES"
RUNTEST_OPTS += -e ghc_with_vanilla=1
else
RUNTEST_OPTS += -e ghc_with_vanilla=0
endif

ifeq "$(HAVE_DYNAMIC)" "YES"
RUNTEST_OPTS += -e ghc_with_dynamic=1
else
RUNTEST_OPTS += -e ghc_with_dynamic=0
endif

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

ifeq "$(GhcWithInterpreter)" "NO"
RUNTEST_OPTS += -e ghc_with_interpreter=0
else ifeq "$(GhcStage)" "1"
RUNTEST_OPTS += -e ghc_with_interpreter=0
else
RUNTEST_OPTS += -e ghc_with_interpreter=1
endif

ifeq "$(GhcUnregisterised)" "YES"
RUNTEST_OPTS += -e ghc_unregisterised=1
else
RUNTEST_OPTS += -e ghc_unregisterised=0
endif

ifeq "$(GhcDynamicByDefault)" "YES"
RUNTEST_OPTS += -e ghc_dynamic_by_default=True
CABAL_MINIMAL_BUILD = --enable-shared --disable-library-vanilla
else
RUNTEST_OPTS += -e ghc_dynamic_by_default=False
CABAL_MINIMAL_BUILD = --enable-library-vanilla --disable-shared
endif

ifeq "$(GhcDynamic)" "YES"
RUNTEST_OPTS += -e ghc_dynamic=True
CABAL_PLUGIN_BUILD = --enable-shared --disable-library-vanilla
else
RUNTEST_OPTS += -e ghc_dynamic=False
CABAL_PLUGIN_BUILD = --enable-library-vanilla --disable-shared
endif

ifeq "$(GhcWithSMP)" "YES"
RUNTEST_OPTS += -e ghc_with_smp=1
else
RUNTEST_OPTS += -e ghc_with_smp=0
endif

ifneq "$(shell $(SHELL) -c 'llc --version | grep version' 2> /dev/null)" ""
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

ifneq "$(CLEAN_ONLY)" ""
RUNTEST_OPTS += -e clean_only=True
else
RUNTEST_OPTS += -e clean_only=False
endif

ifneq "$(CHECK_FILES_WRITTEN)" ""
RUNTEST_OPTS += --check-files-written
endif

RUNTEST_OPTS +=  \
	--rootdir=. \
	--config=$(CONFIG) \
	-e 'config.confdir="$(CONFIGDIR)"' \
	-e 'config.compiler="$(TEST_HC)"' \
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
	-e 'config.top="$(TOP_ABS)"'

ifneq "$(OUTPUT_SUMMARY)" ""
RUNTEST_OPTS +=  \
	--output-summary "$(OUTPUT_SUMMARY)"
endif

RUNTEST_OPTS +=  \
	$(EXTRA_RUNTEST_OPTS)

ifeq "$(list_broken)" "YES"
set_list_broken = -e config.list_broken=True
else
set_list_broken = 
endif

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
		$(set_list_broken) \
		$(setfast) \
		$(setaccept)

verbose: test

accept:
	$(MAKE) accept=YES

fast:
	$(MAKE) fast=YES

list_broken:
	$(MAKE) list_broken=YES

