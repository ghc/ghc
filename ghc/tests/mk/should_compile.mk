#-----------------------------------------------------------------------------
# template for should_compile tests.

HS_SRCS = $(wildcard *.hs)

SRC_RUNTEST_OPTS += -o1 $*.stdout -o2 $*.stderr -x 0

%.o : %.hs
	@echo ---- Testing for successful compilation of $<
	@$(RUNTEST) $(HC) $(RUNTEST_OPTS) -- $(HC_OPTS) -c $< -o $@

all :: $(HS_OBJS)

