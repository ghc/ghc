TOP = ../../../..
include $(TOP)/mk/boilerplate.mk

HS_SRCS = $(wildcard *.hs)
BINS = $(patsubst %.o,%,$(HS_OBJS))

SRC_RUNTEST_OPTS += -o1 $*.stdout -o2 $*.stderr -x 0
HC_OPTS += -noC -dcore-lint

all :: $(BINS)

%.o : %
	$(HC) $(HC_OPTS) $< -o $@


include $(TOP)/mk/target.mk


