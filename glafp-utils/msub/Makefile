TOP=../..
include $(TOP)/mk/gen.mk

SRCS=msub.c
PROG=msub
DESTDIR=$(INSTBINDIR)

# No need for dependencies, which is just as well since
# mkdependC can't be built until msub has been build!
OmitCDepend = YES

include $(TOP)/mk/Cprog.mk
