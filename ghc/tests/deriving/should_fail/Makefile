TOP = ../../../..
include $(TOP)/mk/boilerplate.mk

HS_SRCS = $(wildcard *.hs)

SRC_RUNTEST_OPTS += -o1 $*.stdout -o2 $*.stderr -x 1
HC_OPTS += -noC

%.o : %.hs
	$(RUNTEST) $(HC) $(RUNTEST_OPTS) -- $(HC_OPTS) -c $< -o $@

all :: $(HS_OBJS)

include $(TOP)/mk/target.mk
