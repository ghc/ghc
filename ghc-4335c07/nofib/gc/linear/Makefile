TOP = ../..

include $(TOP)/mk/boilerplate.mk

NORM_OPTS = 14

ifeq "$(HEAP)" "LARGE"
SRC_RUNTEST_OPTS += +RTS -H8m -RTS
endif
ifeq "$(HEAP)" "OLD"
SRC_RUNTEST_OPTS += +RTS -H10m -RTS
endif

include $(TOP)/mk/target.mk
