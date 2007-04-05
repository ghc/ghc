TOP = ..
include $(TOP)/mk/boilerplate.mk

HC = $(GHC_INPLACE)
MKDEPENDHS = $(GHC_INPLACE)
SRC_HC_OPTS += -threaded
EXCLUDED_SRCS += Calibrate.hs

ifeq "$(Windows)" "NO"
SRC_HC_OPTS += -package unix
endif

HS_PROG = timeout

boot :: calibrate.out

ifeq "$(findstring thr,$(GhcRTSWays))" "thr"
boot :: $(HS_PROG)
else
boot :: python-timeout
endif

calibrate.out:
	chmod +x calibrate
	rm -f Calibrate.o Calibrate.hi Calibrate
	./calibrate "$(HC)" > $@

python-timeout:
	cp timeout.py timeout
	chmod +x timeout

include $(TOP)/mk/target.mk
