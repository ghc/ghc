TOP = ../../../..
include $(TOP)/mk/boilerplate.mk

HS_SRCS = $(wildcard *.hs)

SRC_RUNTEST_OPTS += -o1 $*.stdout -o2 $*.stderr -x 1
HC_OPTS += -noC -ddump-tc

%.o : %.hs
	$(RUNTEST) $(HC) $(RUNTEST_OPTS) -- $(HC_OPTS) -c $< -o $@

all :: $(HS_OBJS)

tcfail045_HC_OPTS = -fglasgow-exts
tcfail059_HC_OPTS = -hi		     
tcfail060_HC_OPTS = -hi
tcfail061_HC_OPTS = -hi
tcfail062_HC_OPTS = -hi
tcfail063_HC_OPTS = -hi
tcfail064_HC_OPTS = -hi
tcfail065_HC_OPTS = -hi
tcfail066_HC_OPTS = -hi
tcfail067_HC_OPTS = -hi
tcfail068_HC_OPTS = -fglasgow-exts

include $(TOP)/mk/target.mk
