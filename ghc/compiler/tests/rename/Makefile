TOP = ../../..
include $(TOP)/mk/boilerplate.mk

HS_SRCS = $(wildcard *.hs)

SRC_RUNTEST_OPTS += -o1 $*.stdout -o2 $*.stderr -x 0
HC_OPTS += -noC -ddump-rn -dcore-lint

#SUBDIRS = bevan-bug-1

rn001_HC_OPTS = -x 1
rn002_HC_OPTS = -x 1
rn004_HC_OPTS = -x 1
rn007_HC_OPTS = -x 1
rn008_HC_OPTS = -x 1
rn014_HC_OPTS = -x 1
rn015_HC_OPTS = -x 1
rn017_HC_OPTS = -hi

%.o : %.hs

%.o : %.hs
	$(RUNTEST) $(HC) $(RUNTEST_OPTS) -- $(HC_OPTS) -c $< -o $@ -osuf $(subst .,,$(suffix $@)) 

all :: $(HS_OBJS)

include $(TOP)/mk/target.mk


