TOP=../..
include $(TOP)/mk/boilerplate.mk

# Exclude for booting
ifeq "$(stage)" "2"
SRC_CC_OPTS += $(MACOSX_DEPLOYMENT_CC_OPTS)
SRC_LD_OPTS += $(MACOSX_DEPLOYMENT_LD_OPTS)
endif

C_SRCS=unlit.c
C_PROG=unlit
SRC_CC_OPTS += -O

# Get it over with!
boot :: all

#
# Install unlit in lib/
#
INSTALL_LIBEXECS += $(C_PROG)

BINDIST_EXTRAS += $(C_PROG)
include $(TOP)/mk/bindist.mk

include $(TOP)/mk/target.mk
