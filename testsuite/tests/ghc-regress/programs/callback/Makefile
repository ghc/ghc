#-----------------------------------------------------------------------------
# $Id

TOP = ..
include $(TOP)/mk/boilerplate.mk

SRC_HC_OPTS += -fglasgow-exts -fvia-C -package lang
SRC_LD_OPTS += Main_stub.o

all :: runtest

include $(TOP)/mk/test.mk
