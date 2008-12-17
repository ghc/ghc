TESTSUITE_TOP := $(TOP)
TOP:=$(TOP)/..

ifneq "$(wildcard $(TOP)/ghc.mk)" ""
NEWBUILD=YES
else
NEWBUILD=NO
endif

ifeq "$(NEWBUILD)" "YES"

FPTOOLS_TOP:=$(TOP)
include $(FPTOOLS_TOP)/mk/newconfig.mk

default : all

else

include $(TOP)/mk/boilerplate.mk

endif

TOP:=$(TESTSUITE_TOP)
