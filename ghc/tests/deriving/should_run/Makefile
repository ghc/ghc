TOP = ../..
include $(TOP)/mk/boilerplate.mk

HS_SRCS = $(wildcard *.hs)
BINS = $(patsubst %.o,%,$(HS_OBJS))
RUNTESTS = $(patsubst %,%.run,$(BINS))

SRC_RUNTEST_OPTS += -o1 $*.stdout -o2 $*.stderr -x 0
HC_OPTS += -dcore-lint

all :: $(RUNTESTS)

%.run : %
	$(RUNTEST) $< $(RUNTEST_OPTS)

% : %.o
	$(HC) $(HC_OPTS) $< -o $@

include $(TOP)/mk/target.mk
