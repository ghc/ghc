TOP = ../..
include $(TOP)/mk/boilerplate.mk

FAST_OPTS = 7
# NORM_OPTS should probably be 8 or 9
NORM_OPTS = 11
SLOW_OPTS = 11

ifeq "$(HEAP)" "LARGE"
SRC_RUNTEST_OPTS += +RTS -H330m -RTS
endif
ifeq "$(HEAP)" "OLD"
SRC_RUNTEST_OPTS += +RTS -H10m -RTS
endif

include $(TOP)/mk/target.mk
