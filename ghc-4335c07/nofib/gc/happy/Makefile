TOP = ../..

include $(TOP)/mk/boilerplate.mk

NORM_OPTS = TestInput.y
SRC_HC_OPTS += -cpp -package containers
EXCLUDED_SRCS += TestInput.hs

ifeq "$(HEAP)" "LARGE"
SRC_RUNTEST_OPTS += +RTS -H128m -RTS
endif
ifeq "$(HEAP)" "OLD"
SRC_RUNTEST_OPTS += +RTS -H10m -RTS
endif

include $(TOP)/mk/target.mk
