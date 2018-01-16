TOP = ../..
include $(TOP)/mk/boilerplate.mk

# Override default SRCS; the default is all source files, but
# we don't want to include paraffins.c
SRCS=Main.hs
SRC_RUNTEST_OPTS += +RTS -H20m -RTS

include $(TOP)/mk/target.mk


FAST_OPTS = 17
NORM_OPTS = 17
SLOW_OPTS = 19
