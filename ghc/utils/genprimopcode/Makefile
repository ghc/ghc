TOP=../..
include $(TOP)/mk/boilerplate.mk

INSTALL_PROGS += genprimopcode

SRC_HC_OPTS += -syslib text
OBJS = Main.o

CLEAN_FILES += genprimopcode

# genprimopcode is needed to boot in ghc/compiler...
boot :: all

all :: genprimopcode

genprimopcode: Main.o
	$(HC) -o $@ $(HC_OPTS) $(LD_OPTS) $(OBJS)

CLEAN_FILES += genprimopcode
CLEAN_FILES += $(OBJS)

include $(TOP)/mk/target.mk
