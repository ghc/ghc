TOP = ../../..
include $(TOP)/mk/boilerplate.mk

HS_SRCS = $(wildcard *.hs)

SRC_RUNTEST_OPTS += -o1 $*.stdout -o2 $*.stderr -x 0
HC_OPTS += -dcore-lint -fglasgow-exts

cc002_RUNTEST_OPTS = -x 1
cc004_RUNTEST_OPTS = -x 1
cc007_RUNTEST_OPTS = -x 1

# Note that these tests are still in a state of flux... don't believe errors
# they report. In fact, these aren't really very good tests at all...

cc001_HC_OPTS = -noC -ddump-tc -ddump-ds 
cc002_HC_OPTS = -noC -ddump-tc -ddump-ds 
cc003_HC_OPTS = -noC -ddump-tc -ddump-ds
cc004_HC_OPTS = -noC -ddump-tc -ddump-ds 
cc005_HC_OPTS = -fvia-C -ddump-stg -ddump-flatC
cc006_HC_OPTS = -fvia-C -ddump-stg -ddump-flatC
cc007_HC_OPTS = -fvia-C -ddump-stg -ddump-flatC


%.o : %.hs

%.o : %.hs
	$(RUNTEST) $(HC) $(RUNTEST_OPTS) $(HC_OPTS) -c $< -o $@ -osuf $(subst .,,$(suffix $@)) 

all :: $(HS_OBJS)

include $(TOP)/mk/target.mk
