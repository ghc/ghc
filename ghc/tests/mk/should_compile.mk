#-----------------------------------------------------------------------------
# $Id: should_compile.mk,v 1.5 2000/12/12 13:57:40 simonmar Exp $
# template for should_compile tests.

HS_SRCS = $(wildcard *.hs)

SRC_RUNTEST_OPTS += -x 0 \
	$(foreach i,$(wildcard $*.stdout),-o1 $(i)) \
	$(foreach i,$(wildcard $*.stderr),-o2 $(i))

%.o : %.hs
	@echo ---- Testing for successful compilation of $<
	$(RUNTEST) $(HC) $(RUNTEST_OPTS) -- $(HC_OPTS) -c $< -o $@

%.hc : %.hs
	@echo ---- Testing for successful compilation of $<
	$(RUNTEST) $(HC) $(RUNTEST_OPTS) -- $(HC_OPTS) -C $< -o $@

ifeq "$(HCOnly)" "YES"
all :: $(HS_HCS)
else
all :: $(HS_OBJS)
endif

# Most single-module tests are declared to be module ShouldCompile, so we
# can clean the .hi files in one go:
CLEAN_FILES += ShouldCompile.hi
