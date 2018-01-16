TOP=../..
include $(TOP)/mk/boilerplate.mk

NORM_OPTS = 1 2 4000 1000 1001 4000

ifeq "$(HEAP)" "LARGE"
SRC_RUNTEST_OPTS += +RTS -H256m -RTS
endif

include $(TOP)/mk/target.mk
