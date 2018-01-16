TOP = ../..
include $(TOP)/mk/boilerplate.mk

ifeq "$(HEAP)" "LARGE"
SRC_RUNTEST_OPTS += +RTS -H16m -RTS
endif
ifeq "$(HEAP)" "OLD"
SRC_RUNTEST_OPTS += +RTS -H10m -RTS
endif

include $(TOP)/mk/target.mk

