TOP = ../..
include $(TOP)/mk/boilerplate.mk

HS_SRCS = $(wildcard *.hs)
BINS = $(patsubst %.o,%,$(HS_OBJS))
RUNTESTS = $(patsubst %,%.run,$(BINS))

SRC_RUNTEST_OPTS += -o1 $*.stdout -o2 $*.stderr -x 0
HC_OPTS += -dcore-lint

all :: $(BINS) $(RUNTESTS)

%.run : %
	$(RUNTEST) $< $(RUNTEST_OPTS)

% : %.o
	$(HC) $(HC_OPTS) $< -o $@

cg012_HC_OPTS = -fglasgow-exts
cg015_HC_OPTS = -fglasgow-exts
cg018_HC_OPTS = -fglasgow-exts
cg026_HC_OPTS = -fglasgow-exts
cg029_HC_OPTS = -fglasgow-exts
cg030_HC_OPTS = -fglasgow-exts
cg031_HC_OPTS = -fglasgow-exts
cg032_HC_OPTS = -fglasgow-exts
cg033_HC_OPTS = -fglasgow-exts
cg035_HC_OPTS = -fglasgow-exts
cg042_HC_OPTS = -fglasgow-exts

cg016_RUNTEST_OPTS = -x 1	# should override the -x 0 above.
cg021_RUNTEST_OPTS = +RTS -K2m
cg025_RUNTEST_OPTS = -x 1 cg025.hs

include $(TOP)/mk/target.mk
