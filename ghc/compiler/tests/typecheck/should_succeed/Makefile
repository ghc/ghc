TOP = ../../../..
include $(TOP)/mk/boilerplate.mk

HS_SRCS = $(wildcard *.hs)

SRC_RUNTEST_OPTS += -o1 $*.stdout -o2 $*.stderr -x 0
HC_OPTS += -noC -ddump-tc -dcore-lint -hi

# Expect failure.  Why aren't they in "should-fail"?
tc075_RUNTEST_OPTS += -x 1
tc080_RUNTEST_OPTS += -x 1

%.o : %.hs

%.o : %.hs
	$(RUNTEST) $(HC) $(RUNTEST_OPTS) -- $(HC_OPTS) -c $< -o $@ 

all :: $(HS_OBJS)

tc019_HC_OPTS = -fglasgow-exts
tc065_HC_OPTS = -syslib ghc

include $(TOP)/mk/target.mk


