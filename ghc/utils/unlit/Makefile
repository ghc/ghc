TOP=../..
include $(TOP)/mk/boilerplate.mk

C_SRCS=unlit.c
C_PROG=unlit
SRC_CC_OPTS += -O

# Get it over with!
boot :: all

#
# Install unlit in lib/
#
INSTALL_LIBEXECS += $(C_PROG)

include $(TOP)/mk/target.mk
