TOP = ..
include $(TOP)/mk/boilerplate.mk

SRC_RUNTEST_OPTS += test_data 
SRC_HC_OPTS += -cpp -syslib misc
EXTRA_LD_OPTS += -syslib misc

OBJS = $(HS_OBJS)

all :: runtest

include $(TOP)/mk/target.mk

