#-----------------------------------------------------------------------------
# template for should_run tests.

SCRIPTS = $(wildcard *.script)

SRC_RUNTEST_OPTS += -x 0 \
	$(foreach i,$(wildcard $*.stdout),-o1 $(i)) \
	$(foreach i,$(wildcard $*.stderr),-o2 $(i))

RUNTESTS = $(filter-out $(OMITTED_RUNTESTS), $(patsubst %.script,%.run,$(SCRIPTS)))

all :: $(RUNTESTS)

%.run : %.script
	HC=$(HC) HC_OPTS="$(HC_OPTS)" $(RUNTEST) $(HC) -i$< $(RUNTEST_OPTS) -- --interactive -ignore-dot-ghci

include $(TOP)/mk/target.mk
