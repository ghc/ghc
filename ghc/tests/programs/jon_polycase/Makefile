TOP = ../..
include $(TOP)/mk/boilerplate.mk

# The bug was delicate: only happens without -O
SRC_HC_OPTS += -Onot

# Just compiling Foo successfully is OK
all :: Foo.o

include $(TOP)/mk/target.mk

