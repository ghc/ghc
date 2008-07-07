TOP = ..
include $(TOP)/mk/boilerplate.mk

HC = $(GHC_INPLACE)
BOOTSTRAPPING_PACKAGE_CONF_HC_OPTS=

MKDEPENDHS = $(GHC_INPLACE)
SRC_HC_OPTS += -threaded
EXCLUDED_SRCS += TimeMe.hs

ifeq "$(Windows)" "YES"
SRC_HC_OPTS += -package Win32
else
SRC_HC_OPTS += -package unix
endif

all :: calibrate.out

ifeq "$(findstring thr,$(GhcRTSWays))" "thr"
HS_PROG = timeout
else
all :: python-timeout
CLEAN_FILES += timeout
endif

boot :: all

calibrate.out:
	rm -f TimeMe.o TimeMe.hi TimeMe
	$(PYTHON) calibrate "$(HC)" > $@

CLEAN_FILES += calibrate.out

python-timeout:
	cp timeout.py timeout
	chmod +x timeout

include $(TOP)/mk/target.mk
