TOP = ../../..
include $(TOP)/mk/boilerplate.mk


HS_SRCS = $(wildcard *.hs)
SRC_RUNTEST_OPTS += -accept-output -o1 $*.stdout -o2 $*.stderr -x 1
HC_OPTS += -noC -dcore-lint

read002_RUNTEST_OPTS = -x 0

%.o : %.hs

%.o : %.hs
	$(RUNTEST) $(HC) $(RUNTEST_OPTS) $(HC_OPTS) -c $< -o $@ -osuf $(subst .,,$(suffix $@)) 

all :: $(HS_OBJS)

read001_HC_OPTS = -noC -ddump-rdr
read002_HC_OPTS = -noC -ddump-rdr
read004_HC_OPTS = -noC -fno-implicit-prelude -ddump-rdr

include $(TOP)/mk/target.mk
