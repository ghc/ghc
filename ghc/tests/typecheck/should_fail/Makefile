TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_fail.mk

HS_SRCS = $(wildcard *.hs)

# The -noC messes up the Data82 compilation
# HC_OPTS += -noC

tcfail045_HC_OPTS = -fglasgow-exts
tcfail068_HC_OPTS = -fglasgow-exts
tcfail080_HC_OPTS = -fglasgow-exts

Inst82_1.o : Inst82_1.hs Data82.hi
	$(HC) $(HC_OPTS) -c $< -o $@
Inst82_2.o : Inst82_2.hs Data82.hi
	$(HC) $(HC_OPTS) -c $< -o $@
Data82.o : Data82.hs
	$(HC) $(HC_OPTS) -c $< -o $@

# mkdependHS doesn't understand OPTIONS pragmas...
SRC_MKDEPENDHS_OPTS += -fglasgow-exts

include $(TOP)/mk/target.mk
