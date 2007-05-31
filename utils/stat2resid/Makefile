TOP=../..
include $(TOP)/mk/boilerplate.mk

DYN_LOADABLE_BITS = \
	parse-gcstats.prl \
	process-gcstats.prl

SCRIPT_PROG=stat2resid
SCRIPT_OBJS=stat2resid.prl

#
# Could be overridden from the cmd line (see install rule below).
#
INSTALLING=0

INTERP=perl

#
# install setup
# 
INSTALL_LIBS    += $(DYN_LOADABLE_BITS)
INSTALL_SCRIPTS += $(SCRIPT_PROG)


#
# Before really installing the script, we have to
# reconfigure it such that the paths it refers to,
# point to the installed utils.
#
install ::
	$(RM) $(SCRIPT_PROG)
	$(MAKE) $(MFLAGS) INSTALLING=1 $(SCRIPT_PROG)

include $(TOP)/mk/target.mk

# Hack to re-create the in-situ build tree script after 
# having just installed it.
#
install ::
	@$(RM) $(SCRIPT_PROG)
	@$(MAKE) $(MFLAGS) $(SCRIPT_PROG)
