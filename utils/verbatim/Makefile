TOP=../..
include $(TOP)/mk/boilerplate.mk

C_SRCS = verbatim.c
C_PROG = verbatim
LIBS = $(FLEX_LIB)

override SRC_FLEX_OPTS=-8

#
# For src distributions, include flex output.
#
SRC_DIST_FILES += verbatim.c

CLEAN_FILES += verbatim.c

include $(TOP)/mk/target.mk
