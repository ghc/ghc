TOP = ../../..
include $(TOP)/mk/boilerplate.mk

HS_SRCS = $(wildcard *.lhs)

SRC_RUNTEST_OPTS += -accept-output -o1 $*.stdout -o2 $*.stderr -x 0
HC_OPTS += -noC -O -ddump-simpl -dcore-lint -dppr-user

%.o : %.lhs

%.o : %.lhs
	$(RUNTEST) $(HC) $(RUNTEST_OPTS) -- $(HC_OPTS) -c $< -o $@ -osuf $(subst .,,$(suffix $@)) 

all :: $(HS_OBJS)

include $(TOP)/mk/target.mk
