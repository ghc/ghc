TOP=..
include $(TOP)/mk/boilerplate.mk

C_SRCS = sgmlverb.c
C_PROG = sgmlverb
LIBS = $(FLEX_LIB)

override SRC_FLEX_OPTS=-8

# sgmlverb.c isn't in distclean
MAINTAINER_CLEAN_FILES += sgmlverb.c

#
# For src distributions, include flex output.
#
SRC_DIST_FILES += sgmlverb.c

include $(TOP)/mk/target.mk
