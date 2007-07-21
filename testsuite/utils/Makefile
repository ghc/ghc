TOP = ..
include $(TOP)/mk/boilerplate.mk

HC = $(GHC_INPLACE)

all ::
	$(HC) --make pwd

clean ::
	rm -f *.o
	rm -f *.hi
	rm -f pwd

include $(TOP)/mk/target.mk
