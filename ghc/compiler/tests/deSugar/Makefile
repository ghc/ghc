TOP = ../../..
include $(TOP)/mk/boilerplate.mk

HS_SRCS = $(wildcard *.hs)
SRC_RUNTEST_OPTS += -o1 $*.stdout -o2 $*.stderr -x 0

ds014a_RUNTEST_OPTS = -x 1
ds025_RUNTEST_OPTS  = -x 1
ds035_RUNTEST_OPTS  = -x 1
ds038_RUNTEST_OPTS  = -x 1
ds040_RUNTEST_OPTS  = -x 1

%.o : %.hs

%.o : %.hs
	$(RUNTEST) $(HC) $(RUNTEST_OPTS) $(HC_OPTS) -c $< -o $@ -osuf $(subst .,,$(suffix $@)) 

all :: $(HS_OBJS)

#SUBDIRS = cvh-ds-unboxed

# These options apply to all tests
HC_OPTS += -noC -ddump-ds -dcore-lint

ds030_HC_OPTS = -dppr-all
ds035_HC_OPTS = -fglasgow-exts
ds039_HC_OPTS = -dppr-all

include $(TOP)/mk/target.mk
