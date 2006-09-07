TOP = ..
include $(TOP)/mk/boilerplate.mk

HC = $(GHC_INPLACE)
MKDEPENDHS = $(GHC_INPLACE)
SRC_HC_OPTS += -threaded

ifeq "$(Windows)" "NO"
SRC_HC_OPTS += -package unix
endif

HS_PROG = timeout

ifeq "$(findstring thr,$(GhcRTSWays))" "thr"
boot :: $(HS_PROG)
else
boot :: python-timeout
endif

python-timeout:
	cp timeout.py timeout
	chmod +x timeout

include $(TOP)/mk/target.mk
