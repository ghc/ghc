#-----------------------------------------------------------------------------
# template for should_fail tests

HS_SRCS = $(wildcard *.hs)

SRC_RUNTEST_OPTS += -o1 $*.stdout -o2 $*.stderr -x 1

%.o : %.hs
	@echo \*\*\* Testing for failure to compile $<
	@$(RUNTEST) $(HC) $(RUNTEST_OPTS) -- $(HC_OPTS) -c $< -o $@

all :: $(HS_OBJS)
