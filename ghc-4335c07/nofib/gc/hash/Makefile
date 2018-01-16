TOP = ../..
include $(TOP)/mk/boilerplate.mk

FAST_OPTS = 1000000
NORM_OPTS = 10000000
SLOW_OPTS = 100000000

ifeq "$(HEAP)" "LARGE"
SRC_RUNTEST_OPTS += +RTS -H430m -RTS
endif

include $(TOP)/mk/target.mk
