TOP=../..
include $(TOP)/mk/boilerplate.mk

INSTALL_PROGS += genprimopcode

SRC_HC_OPTS += -syslib text
OBJS = Main.o

CLEAN_FILES += genprimopcode

all :: genprimopcode

genprimopcode: Main.o
	$(HC) -o $@ $(HC_OPTS) $(LD_OPTS) $(OBJS)

CLEAN_FILES += genprimopcode
CLEAN_FILES += $(OBJS)

include $(TOP)/mk/target.mk
