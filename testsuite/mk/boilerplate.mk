
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

define get-ghc-rts-field # $1 = result variable, $2 = field name
$1 := $$(shell '$$(TEST_HC)' +RTS --info | grep '^ .("$2",' | tr -d '\r' | sed -e 's/.*", *"//' -e 's/")$$$$//')
endef

define get-ghc-field # $1 = result variable, $2 = field name
$1 := $$(shell '$$(TEST_HC)' --info | grep '^ .("$2",' | tr -d '\r' | sed -e 's/.*", *"//' -e 's/")$$$$//')
endef

define get-ghc-feature-bool # $1 = result variable, $2 = field name
SHELL_RES := $$(shell '$$(TEST_HC)' --info | grep '^ .("$2",' | tr -d '\r' | sed -e 's/.*", *"//' -e 's/")$$$$//')
$1 := $$(strip \
	  $$(if $$(SHELL_RES), \
         $$(if $$(subst YES,,$$(SHELL_RES)), \
            $$(if $$(subst NO,,$$(SHELL_RES)), \
               $$(warning ghc info field not YES or NO: $2: $$(SHELL_RES)), \
               NO), \
            YES), \
         $$(warning ghc info field not found: $2)))
endef

ifeq "$(TEST_HC)" ""

STAGE1_GHC := $(abspath $(TOP)/../inplace/bin/ghc-stage1)
STAGE2_GHC := $(abspath $(TOP)/../inplace/bin/ghc-stage2)
STAGE3_GHC := $(abspath $(TOP)/../inplace/bin/ghc-stage3)

ifneq "$(wildcard $(STAGE1_GHC) $(STAGE1_GHC).exe)" ""

IN_TREE_COMPILER = YES
ifeq "$(BINDIST)" "YES"
TEST_HC := $(abspath $(TOP)/../)/bindisttest/install dir/bin/ghc
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
# so we can just always 'which' it. We need to use 'override' in order
# to override a value given on the commandline.
override TEST_HC := $(shell which '$(TEST_HC)')
endif

# We can't use $(dir ...) here as TEST_HC might be in a path
# containing spaces
BIN_ROOT = $(shell dirname '$(TEST_HC)')

ifeq "$(GHC_PKG)" ""
GHC_PKG := $(BIN_ROOT)/ghc-pkg
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

$(eval $(call get-ghc-field,GhcRTSWays,RTS ways))

TOP_ABS := $(abspath $(TOP))
$(eval $(call canonicalise,TOP_ABS))

GS = gs
CP = cp
RM = rm -f
PYTHON = python

$(eval $(call get-ghc-rts-field,HostOS,Host OS))
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

