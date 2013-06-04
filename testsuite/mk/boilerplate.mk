
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
$1_CYGPATH := $$(shell $(SHELL) -c "cygpath -m '$$($1)'" 2> /dev/null)
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

STAGE1_GHC := $(abspath $(TOP)/../inplace/bin/ghc-stage1)
STAGE2_GHC := $(abspath $(TOP)/../inplace/bin/ghc-stage2)
STAGE3_GHC := $(abspath $(TOP)/../inplace/bin/ghc-stage3)

ifneq "$(wildcard $(STAGE1_GHC) $(STAGE1_GHC).exe)" ""

IN_TREE_COMPILER = YES
ifeq "$(BINDIST)" "YES"
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
IN_TREE_COMPILER = NO
TEST_HC := $(shell which ghc)
endif

else
IN_TREE_COMPILER = NO
# We want to support both "ghc" and "/usr/bin/ghc" as values of TEST_HC
# passed in by the user, but
#     which ghc          == /usr/bin/ghc
#     which /usr/bin/ghc == /usr/bin/ghc
# so on unix-like platforms we can just always 'which' it.
# However, on cygwin, we can't just use which:
#     $ which c:/ghc/ghc-7.4.1/bin/ghc.exe
#     which: no ghc.exe in (./c:/ghc/ghc-7.4.1/bin)
# so we start off by using realpath, and if that succeeds then we use
# that value. Otherwise we fall back on 'which'.
#
# Note also that we need to use 'override' in order to override a
# value given on the commandline.
TEST_HC_REALPATH := $(realpath $(TEST_HC))
ifeq "$(TEST_HC_REALPATH)" ""
override TEST_HC := $(shell which '$(TEST_HC)')
else
override TEST_HC := $(TEST_HC_REALPATH)
endif
endif

# We can't use $(dir ...) here as TEST_HC might be in a path
# containing spaces
BIN_ROOT = $(shell dirname '$(TEST_HC)')

ifeq "$(GHC_PKG)" ""
GHC_PKG := $(BIN_ROOT)/ghc-pkg
endif

ifeq "$(RUNGHC)" ""
RUNGHC := $(BIN_ROOT)/runghc
endif

ifeq "$(HSC2HS)" ""
HSC2HS := $(BIN_ROOT)/hsc2hs
endif

ifeq "$(HP2PS_ABS)" ""
HP2PS_ABS := $(BIN_ROOT)/hp2ps
endif

ifeq "$(HPC)" ""
HPC := $(BIN_ROOT)/hpc
endif

$(eval $(call canonicaliseExecutable,TEST_HC))
ifeq "$(shell test -x '$(TEST_HC)' && echo exists)" ""
$(error Cannot find ghc: $(TEST_HC))
endif

$(eval $(call canonicaliseExecutable,GHC_PKG))
ifeq "$(shell test -x '$(GHC_PKG)' && echo exists)" ""
$(error Cannot find ghc-pkg: $(GHC_PKG))
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
PYTHON = python
ifeq "$(shell $(SHELL) -c 'python2 -c 0' 2> /dev/null && echo exists)" "exists"
PYTHON = python2
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
ghc-config-mk = $(TOP)/mk/ghcconfig$(subst $(space),_,$(subst :,_,$(subst /,_,$(subst \,_,$(TEST_HC))))).mk

$(ghc-config-mk) : $(TOP)/mk/ghc-config
	$(TOP)/mk/ghc-config "$(TEST_HC)" >"$@"; if [ $$? != 0 ]; then $(RM) "$@"; exit 1; fi
# If the ghc-config fails, remove $@, and fail

ifeq "$(findstring clean,$(MAKECMDGOALS))" ""
include $(ghc-config-mk)
endif

ifeq "$(GhcDynamic)" "YES"
ghcThWayFlags     = -dynamic
ghciWayFlags      = -dynamic
ghcPluginWayFlags = -dynamic
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

