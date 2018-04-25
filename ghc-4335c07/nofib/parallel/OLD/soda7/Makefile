TOP = ../..
include $(TOP)/mk/boilerplate.mk

# Override default SRCS; the default is all source files
SRCS=Main.hs
SRC_RUNTEST_OPTS += 20
SRC_HC_OPTS += -cpp -package concurrent

include $(TOP)/mk/target.mk

