TOP = ../..
include $(TOP)/mk/boilerplate.mk

# Arguments to test program
PROG_ARGS = 27000.1 27000.2

ifeq "$(HEAP)" "LARGE"
SRC_RUNTEST_OPTS += +RTS -H32m -RTS
endif
ifeq "$(HEAP)" "OLD"
SRC_RUNTEST_OPTS += +RTS -H24m -RTS
endif

include $(TOP)/mk/target.mk
