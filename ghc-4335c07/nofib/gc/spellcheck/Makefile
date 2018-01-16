TOP = ../..

include $(TOP)/mk/boilerplate.mk

all boot :: input

input : words
	cat words words words words words words words words words words >$@

SRC_HC_OPTS += -package containers
NORM_OPTS = words input

ifeq "$(HEAP)" "LARGE"
SRC_RUNTEST_OPTS += +RTS -H32m -RTS
endif
ifeq "$(HEAP)" "OLD"
SRC_RUNTEST_OPTS += +RTS -H10m -RTS
endif

include $(TOP)/mk/target.mk
