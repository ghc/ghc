TOP = ../..

include $(TOP)/mk/boilerplate.mk

FAST_OPTS = 8 100
NORM_OPTS = 8 5000
SLOW_OPTS = 8 5000

ifeq "$(HEAP)" "LARGE"
SRC_RUNTEST_OPTS += +RTS -H256m -RTS
endif
ifeq "$(HEAP)" "OLD"
SRC_RUNTEST_OPTS += +RTS -H30m -RTS
endif

include $(TOP)/mk/target.mk
