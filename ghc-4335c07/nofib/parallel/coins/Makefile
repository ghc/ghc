TOP = ../..
include $(TOP)/mk/boilerplate.mk

# This version just counts the results, and runs in constant space:
# SRC_RUNTEST_OPTS += 7 1163

# This version builds a list of the results, and needs a lot of memory:
SRC_RUNTEST_OPTS += 3 873

SRC_HC_OPTS += -package parallel

include $(TOP)/mk/target.mk
