#-----------------------------------------------------------------------------
# $Id: should_fail.mk,v 1.4 2000/11/03 16:23:38 simonmar Exp $
# template for should_fail tests

HS_SRCS = $(wildcard *.hs)

SRC_RUNTEST_OPTS += -x 1 \
	$(foreach i,$(wildcard $*.stdout),-o1 $(i)) \
	$(foreach i,$(wildcard $*.stderr),-o2 $(i))

%.o : %.hs
	@echo ---- Testing for failure to compile $<
	@$(RUNTEST) $(HC) $(RUNTEST_OPTS) -- $(HC_OPTS) -c $< -o $@

all :: $(HS_OBJS)

# occasionally a test goes wrong and compiles by mistake, so...
CLEAN_FILES += ShouldFail.hi
