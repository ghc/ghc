TOP=../..
include $(TOP)/mk/boilerplate.mk

INSTALL_PROGS += diff_gcc_nat

SRC_HC_OPTS += -O
OBJS = Diff_Gcc_Nat.o

CLEAN_FILES += diff_gcc_nat

all :: diff_gcc_nat

diff_gcc_nat: Diff_Gcc_Nat.o
	$(HC) -o $@ $(HC_OPTS) $(LD_OPTS) $(OBJS)

CLEAN_FILES += diff_gcc_nat
CLEAN_FILES += $(OBJS)

include $(TOP)/mk/target.mk
