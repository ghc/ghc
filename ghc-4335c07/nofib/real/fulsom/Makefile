TOP = ../..
include $(TOP)/mk/boilerplate.mk

SRC_RUNTEST_OPTS += -stdout-binary

SRC_HC_OPTS += -cpp

# Bah.hs is a test file, which we don't want in SRCS
EXCLUDED_SRCS = Bah.hs

PROG_ARGS = 7

include $(TOP)/mk/target.mk

