TOP = ..
include $(TOP)/mk/boilerplate.mk

HC = $(GHC_INPLACE)
MKDEPENDHS = $(GHC_INPLACE)
SRC_HC_OPTS += -threaded

HS_PROG = timeout

boot :: $(HS_PROG)

include $(TOP)/mk/target.mk
