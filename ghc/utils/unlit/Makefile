TOP=../..
include $(TOP)/mk/boilerplate.mk
override WAYS=

C_SRCS=unlit.c
C_PROG=unlit

# Get it over with!
boot :: all

#
# Install until in lib/.*
#
INSTALL_LIBEXECS += $(C_PROG)

include $(TOP)/mk/target.mk
