TOP=../..
include $(TOP)/mk/boilerplate.mk

HS_PROG		= runghc$(exeext)
INSTALL_PROGS   += $(HS_PROG)

UseGhcForCc = YES
SRC_MKDEPENDC_OPTS += -I$(GHC_INCLUDE_DIR)

# This causes libghccompat.a to be used:
include $(GHC_COMPAT_DIR)/compat.mk

# This is required because libghccompat.a must be built with
# $(GhcHcOpts) because it is linked to the compiler, and hence
# we must also build with $(GhcHcOpts) here:
SRC_HC_OPTS += $(GhcHcOpts)

all :: runhaskell

runhaskell : $(HS_PROG)
	$(CP) $< runhaskell$(exeext)

CLEAN_FILES += runhaskell

# Only install runhaskell if there isn't already one installed
ifneq "$(findstring install, $(MAKECMDGOALS))" ""
ifeq "$(wildcard $(bindir)/runhaskell)" ""
INSTALL_PROGS += runhaskell$(exeext)
endif
endif

include $(TOP)/mk/target.mk
