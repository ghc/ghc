
default: all

HAVE_EVAL := NO
$(eval HAVE_EVAL := YES)

ifeq "$(HAVE_EVAL)" "NO"
$(error Your make does not support eval. You need GNU make >= 3.80)
endif

show:
	@echo '$(VALUE)="$($(VALUE))"'

define canonicalise
# $1 = path variable
$1_CYGPATH := $$(shell $(SHELL) -c "cygpath -m $$($1)" 2> /dev/null)
ifneq "$$($1_CYGPATH)" ""
$1 := $$($1_CYGPATH)
endif
endef

define canonicaliseExecutable
# $1 = program path variable
ifneq "$$(wildcard $$($1).exe)" ""
$1 := $$($1).exe
endif
$(call canonicalise,$1)
endef

define get-ghc-rts-field # $1 = rseult variable, $2 = field name
$1 := $$(shell $$(TEST_HC) +RTS --info | grep '^ .("$2",' | sed -e 's/.*", *"//' -e 's/")$$$$//')
endef

define get-ghc-field # $1 = rseult variable, $2 = field name
$1 := $$(shell $$(TEST_HC) --info | grep '^ .("$2",' | sed -e 's/.*", *"//' -e 's/")$$$$//')
endef

define get-ghc-feature-bool # $1 = rseult variable, $2 = field name
SHELL_RES := $$(shell $$(TEST_HC) --info | grep '^ .("$2",' | sed -e 's/.*", *"//' -e 's/")$$$$//')
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

OLD_BUILD_SYSTEM_STAGE1_GHC := $(abspath $(TOP)/../ghc/stage1-inplace/ghc)
OLD_BUILD_SYSTEM_STAGE2_GHC := $(abspath $(TOP)/../ghc/stage2-inplace/ghc)
OLD_BUILD_SYSTEM_STAGE3_GHC := $(abspath $(TOP)/../ghc/stage3-inplace/ghc)
OLD_BUILD_SYSTEM_GHC_PKG    := $(abspath $(TOP)/../utils/ghc-pkg/install-inplace/bin/ghc-pkg)
OLD_BUILD_SYSTEM_HP2PS      := $(abspath $(TOP)/../utils/hp2ps/hp2ps)
ifneq "$(wildcard $(OLD_BUILD_SYSTEM_STAGE1_GHC) $(OLD_BUILD_SYSTEM_STAGE1_GHC).exe)" ""

ifeq "$(stage)" "1"
TEST_HC := $(OLD_BUILD_SYSTEM_STAGE1_GHC)
else
ifeq "$(stage)" "3"
TEST_HC := $(OLD_BUILD_SYSTEM_STAGE3_GHC)
else
# use stage2 by default
TEST_HC := $(OLD_BUILD_SYSTEM_STAGE2_GHC)
endif
endif
GHC_PKG := $(OLD_BUILD_SYSTEM_GHC_PKG)
HP2PS_ABS := $(OLD_BUILD_SYSTEM_HP2PS)
# XXX This GCC definition is a hack. Once the in-tree GHC has a gcc in the
# right place we won't need to do this, as Cabal will be able to find
# gcc relative to ghc's location.
GCC := $(shell cd $(TOP)/.. && $(MAKE) --no-print-directory show VALUE=WhatGccIsCalled | sed 's/.*"\(.*\)"/\1/')

else
NEW_BUILD_SYSTEM_STAGE1_GHC := $(abspath $(TOP)/../inplace/bin/ghc-stage1)
NEW_BUILD_SYSTEM_STAGE2_GHC := $(abspath $(TOP)/../inplace/bin/ghc-stage2)
NEW_BUILD_SYSTEM_STAGE3_GHC := $(abspath $(TOP)/../inplace/bin/ghc-stage3)
ifneq "$(wildcard $(NEW_BUILD_SYSTEM_STAGE1_GHC) $(NEW_BUILD_SYSTEM_STAGE1_GHC).exe)" ""

ifeq "$(stage)" "1"
TEST_HC := $(NEW_BUILD_SYSTEM_STAGE1_GHC)
else
ifeq "$(stage)" "3"
TEST_HC := $(NEW_BUILD_SYSTEM_STAGE3_GHC)
else
# use stage2 by default
TEST_HC := $(NEW_BUILD_SYSTEM_STAGE2_GHC)
endif
# XXX This GCC definition is a hack. Once the in-tree GHC has a gcc in the
# right place we won't need to do this, as Cabal will be able to find
# gcc relative to ghc's location.
GCC := $(shell cd $(TOP)/.. && $(MAKE) --no-print-directory -s show VALUE=WhatGccIsCalled 2> /dev/null | sed 's/.*"\(.*\)"/\1/')
endif

else
TEST_HC := $(shell which ghc)
endif

endif
endif

ifeq "$(GHC_PKG)" ""
GHC_PKG := $(dir $(TEST_HC))/ghc-pkg
endif

ifeq "$(HP2PS_ABS)" ""
HP2PS_ABS := $(dir $(TEST_HC))/hp2ps
endif

$(eval $(call canonicaliseExecutable,TEST_HC))
ifeq "$(wildcard $(TEST_HC))" ""
$(error Cannot find ghc: $(TEST_HC))
endif

$(eval $(call canonicaliseExecutable,GHC_PKG))
ifeq "$(wildcard $(GHC_PKG))" ""
$(error Cannot find ghc-pkg: $(GHC_PKG))
endif

$(eval $(call canonicaliseExecutable,HP2PS_ABS))
ifeq "$(wildcard $(HP2PS_ABS))" ""
$(error Cannot find hp2ps: $(HP2PS_ABS))
endif

$(eval $(call get-ghc-field,GhcRTSWays,RTS ways))

TOP_ABS := $(abspath $(TOP))
$(eval $(call canonicalise,TOP_ABS))

GS = gs
CP = cp
RM = rm -f
PYTHON = python

