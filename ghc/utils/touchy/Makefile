#
# Substitute for 'touch' on win32 platforms (without an Unix toolset installed).
#
TOP=../..
include $(TOP)/mk/boilerplate.mk

C_SRCS=touchy.c
C_PROG=touchy
SRC_CC_OPTS += -O

# Get it over with!
boot :: all

#
# Install touchy in lib/.*
#
INSTALL_LIBEXECS += $(C_PROG)

include $(TOP)/mk/target.mk
