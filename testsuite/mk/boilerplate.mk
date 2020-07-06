# Don't blindly unexport MAKEFLAGS, see
# Note [Communicating options and variables to a submake].

# Eliminate use of the built-in implicit rules, and clear out the default list
# of suffixes for suffix rules. Speeds up make quite a bit. Both are needed
# for the shortest `make -d` output.
# Don't set --no-builtin-variables; some rules might stop working if you do
# (e.g. 'make clean' in testsuite/ currently relies on an implicit $RM).
MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

default: all

HAVE_EVAL := NO
$(eval HAVE_EVAL := YES)

ifeq "$(HAVE_EVAL)" "NO"
$(error Your make does not support eval. You need GNU make >= 3.81)
endif

ifeq "$(abspath /)" ""
$(error Your make does not support abspath. You need GNU make >= 3.81)
endif

show:
	@echo '$(VALUE)="$($(VALUE))"'

define canonicalise
# $1 = path variable
# Don't use 'cygpath -m', because it doesn't change drive letters to
# something 'which' can understand.
$1_CYGPATH := $$(shell $(SHELL) -c "cygpath '$$($1)'" 2> /dev/null)
ifneq "$$($1_CYGPATH)" ""
# We use 'override' in case we are trying to update a value given on
# the commandline (e.g. TEST_HC)
override $1 := $$($1_CYGPATH)
endif
endef

define canonicaliseExecutable
# $1 = program path variable
ifneq "$$(shell test -x '$$($1).exe' && echo exists)" ""
# We use 'override' in case we are trying to update a value given on
# the commandline (e.g. TEST_HC)
override $1 := $$($1).exe
endif
$(call canonicalise,$1)
endef

ifeq "$(TEST_HC)" ""

# Note [Spaces in TEST_HC]
#
# Tests should be able to handle paths with spaces.
#
# One of the things ./validate (without --fast) does is check if binary
# distributions can successfully be installed and used in paths containing
# spaces.
#
# It does so in the following way:
#    * create a binary distribution in 'bindistprep/'.
#    * install that binary distribution in 'bindisttest/install   dir/'
#    * run the testsuite with BINDIST=YES
#
# BINDIST=YES tells the testsuite driver to use
# 'bindisttest/install   dir/bin/ghc' instead of 'inplace/bin/ghc-stage2' as
# TEST_HC.
#
# Before, if a GHC developer forgot to quote TEST_HC in their Makefile when
# adding a new test, the test would fail with a puzzling "command not found:
# bindisttest/install" error (but only when validating).
#
# Therefore, we now:
#   * make sure 'bindisttest/install' does exist, and show a nice message when
#     it is executed.
#   * let the default value of TEST_HC also contain spaces
#     (i.e. 'inplace/test   spaces/ghc-stage2'), such that the test always
#     fails, also without BINDIST=YES, and again show a nice message when it
#     indeed does so, through 'inplace/test'.

# The `wildcard` function requires spaces to be escaped. Other gnu make
# functions can't seem to handle spaces at all (e.g. `abspath`).
STAGE1_TEST_SPACES := $(TOP)/../inplace/test\ \ \ spaces/ghc-stage1
STAGE1_NORMAL := $(TOP)/../inplace/bin/ghc-stage1

ifneq "$(wildcard $(STAGE1_TEST_SPACES) $(STAGE1_NORMAL))" ""
IMPLICIT_COMPILER = NO
IN_TREE_COMPILER = YES

ifneq "$(wildcard $(STAGE1_TEST_SPACES))" ""
# See Note [Spaces in TEST_HC].
STAGE1_GHC := $(abspath $(TOP)/../)/inplace/test   spaces/ghc-stage1
STAGE2_GHC := $(abspath $(TOP)/../)/inplace/test   spaces/ghc-stage2
STAGE3_GHC := $(abspath $(TOP)/../)/inplace/test   spaces/ghc-stage3
else
# Maybe we're on Windows (no symlink support), or in a bindist or sdist, which
# don't have the 'test   spaces' symlink.
STAGE1_GHC := $(abspath $(TOP)/../)/inplace/bin/ghc-stage1
STAGE2_GHC := $(abspath $(TOP)/../)/inplace/bin/ghc-stage2
STAGE3_GHC := $(abspath $(TOP)/../)/inplace/bin/ghc-stage3
endif

ifeq "$(BINDIST)" "YES"
# See Note [Spaces in TEST_HC].
TEST_HC := $(abspath $(TOP)/../)/bindisttest/install   dir/bin/ghc
else ifeq "$(stage)" "1"
TEST_HC := $(STAGE1_GHC)
else ifeq "$(stage)" "3"
TEST_HC := $(STAGE3_GHC)
else
# use stage2 by default
TEST_HC := $(STAGE2_GHC)
endif

else
IMPLICIT_COMPILER = YES
IN_TREE_COMPILER = NO
TEST_HC := $(shell which ghc)
endif

else # neq "$(TEST_HC)" ""

ifeq "$(TEST_HC)" "ghc"
IMPLICIT_COMPILER = YES
else
IMPLICIT_COMPILER = NO
endif
IN_TREE_COMPILER = NO

# Note [The TEST_HC variable]
#
# As values of TEST_HC passed in by the user, we want to support:
#  * both "ghc" and "/usr/bin/ghc"
#      We use 'which' to convert the former to the latter.
#  * both "C:/path/to/ghc.exe" and "/c/path/to/ghc.exe"
#      We use 'cygpath' to convert the former to the latter, because
#      'which' can't handle paths starting with a drive letter.
#  * paths that contain spaces
#      So we can't use the GNU make function 'realpath'.
# Note also that we need to use 'override' in order to override a
# value given on the commandline.
$(eval $(call canonicaliseExecutable,TEST_HC))
override TEST_HC := $(shell which '$(TEST_HC)')
endif # "$(TEST_HC)" ""

# We can't use $(dir ...) here as TEST_HC might be in a path
# containing spaces
BIN_ROOT = $(shell dirname '$(TEST_HC)')

ifeq "$(IMPLICIT_COMPILER)" "YES"
find_tool = $(shell which $(1))
else
find_tool = $(BIN_ROOT)/$(1)
endif

ifeq "$(GHC_PKG)" ""
GHC_PKG := $(call find_tool,ghc-pkg)
endif

ifeq "$(RUNGHC)" ""
RUNGHC := $(call find_tool,runghc)
endif

ifeq "$(HADDOCK)" ""
HADDOCK := $(call find_tool,haddock)
endif

ifeq "$(HSC2HS)" ""
HSC2HS := $(call find_tool,hsc2hs)
endif

ifeq "$(HP2PS_ABS)" ""
HP2PS_ABS := $(call find_tool,hp2ps)
endif

ifeq "$(HPC)" ""
HPC := $(call find_tool,hpc)
endif

$(eval $(call canonicaliseExecutable,TEST_HC))
ifeq "$(shell test -x '$(TEST_HC)' && echo exists)" ""
$(error Cannot find ghc: $(TEST_HC))
endif

$(eval $(call canonicaliseExecutable,GHC_PKG))
ifeq "$(shell test -x '$(GHC_PKG)' && echo exists)" ""
$(error Cannot find ghc-pkg: $(GHC_PKG))
endif

$(eval $(call canonicaliseExecutable,HADDOCK))
ifeq "$(shell test -x '$(HADDOCK)' && echo exists)" ""
# haddock is optional. Use 'override' to override canonicalise's override...
override HADDOCK :=
endif

$(eval $(call canonicaliseExecutable,HSC2HS))
ifeq "$(shell test -x '$(HSC2HS)' && echo exists)" ""
$(error Cannot find hsc2hs: $(HSC2HS))
endif

$(eval $(call canonicaliseExecutable,HP2PS_ABS))
ifeq "$(shell test -x '$(HP2PS_ABS)' && echo exists)" ""
$(error Cannot find hp2ps: $(HP2PS_ABS))
endif

$(eval $(call canonicaliseExecutable,HPC))
ifeq "$(shell test -x '$(HPC)' && echo exists)" ""
$(error Cannot find hpc: $(HPC))
endif

# Be careful when using this. On Windows it ends up looking like
# c:/foo/bar which confuses make, as make thinks that the : is Makefile
# syntax
TOP_ABS := $(abspath $(TOP))
$(eval $(call canonicalise,TOP_ABS))

GS = gs
CP = cp
RM = rm -f
PYTHON ?= python3

ifeq "$(CHECK_API_ANNOTATIONS)" ""
CHECK_API_ANNOTATIONS := $(abspath $(TOP)/../inplace/bin/check-api-annotations)
endif

ifeq "$(CHECK_PPR)" ""
CHECK_PPR := $(abspath $(TOP)/../inplace/bin/check-ppr)
endif

ifeq "$(CHECK_EXACT)" ""
CHECK_EXACT := $(abspath $(TOP)/../inplace/bin/check-exact)
endif

# -----------------------------------------------------------------------------
# configuration of TEST_HC

# ghc-config.hs is a short Haskell program that runs ghc --info, parses
# the results, and emits a little .mk file with make bindings for the values.
# This way we cache the results for different values of $(TEST_HC)

$(TOP)/mk/ghc-config : $(TOP)/mk/ghc-config.hs
	"$(TEST_HC)" --make -o $@ $<

empty=
space=$(empty) $(empty)
ifeq "$(ghc_config_mk)" ""
ghc_config_mk = $(TOP)/mk/ghcconfig$(subst $(space),_,$(subst :,_,$(subst /,_,$(subst \,_,$(TEST_HC))))).mk

$(ghc_config_mk) : $(TOP)/mk/ghc-config
	$(TOP)/mk/ghc-config "$(TEST_HC)" >"$@"; if [ $$? != 0 ]; then $(RM) "$@"; exit 1; fi
# If the ghc-config fails, remove $@, and fail
endif

# Note: $(CLEANING) is not defined in the testsuite.
ifeq "$(findstring clean,$(MAKECMDGOALS))" ""
-include $(ghc_config_mk)
endif

# Note [WayFlags]
#
# Code that uses TemplateHaskell should either use -fexternal-interpreter, or
# be built in the same way as the compiler (-prof, -dynamic or -static).
#
# We therefore add those flags to ghcThWayFlags and ghc_th_way_flags here and
# in testsuite/config/ghc, and use them in all tests that use TemplateHaskell.
#
# The same applies to code loaded in regular GHCi, and code that uses the
# plugin system.
#
# See #11495 and TEST=TH_spliceE5_prof for a complication: trying to compile
# code that uses TemplateHaskell with -prof, while GhcDynamic=YES.
ifeq "$(GhcDynamic)" "YES"
ghcThWayFlags     = -dynamic
ghciWayFlags      = -dynamic
ghcPluginWayFlags = -dynamic
else ifeq "$(GhcProfiled)" "YES"
ghcThWayFlags     = -prof
ghciWayFlags      = -prof
ghcPluginWayFlags = -prof
else
ghcThWayFlags     = -static
ghciWayFlags      = -static
ghcPluginWayFlags = -static
endif

# -----------------------------------------------------------------------------

ifeq "$(HostOS)" "mingw32"
WINDOWS = YES
else
WINDOWS = NO
endif
ifeq "$(HostOS)" "darwin"
DARWIN = YES
else
DARWIN = NO
endif

