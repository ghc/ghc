#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.6 1997/03/24 02:50:26 sof Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk
override WAYS=
DYN_LOADABLE_BITS = \
	parse-gcstats.prl \
	process-gcstats.prl

SCRIPT_PROG=stat2resid
SCRIPT_OBJS=stat2resid.prl

#
# Could be overridden from the cmd line (see install rule below).
#
INSTALLING=0

SCRIPT_SUBST_VARS=TMPDIR

INTERP=perl
CLEAN_FILES += $(SCRIPT_PROG)

#
# The stat2resid script is configured with a different
# path to the supporting perl scripts, depending on whether it
# is to be installed or not.
#
ifeq "$(INSTALLING)" "1"
ifeq "$(BIN_DIST)" "1"
SCRIPT_PREFIX_FILES += prefix.txt
endif
endif

#
# install setup
# 
INSTALL_LIBS += $(DYN_LOADABLE_BITS)
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
