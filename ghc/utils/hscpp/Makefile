TOP=../..
include $(TOP)/mk/boilerplate.mk

SCRIPT_PROG=hscpp
SCRIPT_OBJS=hscpp.prl
SCRIPT_SUBST_VARS=RAWCPP

# Note: might be overridden from cmd-line (see install rule below)
INSTALLING=0

# no INTERP: do *not* want #! script stuck on the front
# what's the deal? I'll add it for now  -- SOF
INTERP=perl

#
# install setup
#
INSTALL_LIB_SCRIPTS+=$(SCRIPT_PROG)
INSTALL_LIBEXECS=$(C_PROG)


#
# Before really installing the script, we have to
# reconfigure it such that the paths it refers to,
# point to the installed utils.
#
install ::
	@$(RM) $(SCRIPT_PROG)
	@$(MAKE) $(MFLAGS) INSTALLING=1 $(SCRIPT_PROG)


CLEAN_FILES += $(SCRIPT_PROG)

include $(TOP)/mk/target.mk


# Hack to re-create the in-situ build tree script after 
# having just installed it.
#
install ::
	@$(RM) $(SCRIPT_PROG)
	@$(MAKE) $(MFLAGS) BIN_DIST=0 $(SCRIPT_PROG)

