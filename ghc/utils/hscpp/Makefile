TOP=../..
include $(TOP)/mk/boilerplate.mk
WAYS=

SCRIPT_PROG=hscpp
SCRIPT_OBJS=hscpp.prl
SCRIPT_SUBST_VARS=RAWCPP

# no INTERP: do *not* want #! script stuck on the front
# what's the deal? I'll add it for now  -- SOF
INTERP=$(PERL)

#
# install setup
#
INSTALL_LIBEXECS += $(SCRIPT_PROG)
CLEAN_FILES += $(SCRIPT_PROG)

include $(TOP)/mk/target.mk

