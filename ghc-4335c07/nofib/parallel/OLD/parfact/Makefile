TOP = ../..
include $(TOP)/mk/boilerplate.mk
-include opts.mk

# Override default SRCS; the default is all source files
SRCS=parfact.hs
SRC_RUNTEST_OPTS += 8000000 1000
SRC_HC_OPTS += -package concurrent

include $(TOP)/mk/target.mk

