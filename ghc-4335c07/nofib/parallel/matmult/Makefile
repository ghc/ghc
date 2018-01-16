TOP = ../..
include $(TOP)/mk/boilerplate.mk

# 0 : sequential
# 1 : linewise
# 2 : blockwise
# 3 : columnwise

FAST_OPTS = 100 1 10
NORM_OPTS = 600 1 10
SLOW_OPTS = 1000 1 10
SRC_HC_OPTS += -package parallel

# FAST_OPTS = 
# NORM_OPTS = 
# SLOW_OPTS = 

include $(TOP)/mk/target.mk
