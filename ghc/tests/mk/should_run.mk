#-----------------------------------------------------------------------------
# template for should_run tests.

HS_SRCS = $(wildcard *.hs)
BINS = $(patsubst %.o,%.bin,$(HS_OBJS))
RUNTESTS = $(filter-out $(OMITTED_RUNTESTS), $(patsubst %.bin,%.run,$(BINS)))

SRC_RUNTEST_OPTS += -o1 $*.stdout -o2 $*.stderr -x 0

all :: $(RUNTESTS)

%.run : %.bin
	$(RUNTEST) $< $(RUNTEST_OPTS)

%.bin : %.o
	$(HC) $(HC_OPTS) $($*_LD_OPTS) $< -o $@

CLEAN_FILES += $(BINS)
