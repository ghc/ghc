#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.3 1997/03/13 09:10:17 sof Exp $

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

SCRIPT_SUBST_VARS=TMPDIR SCRIPT_PATH

INTERP=$(PERL)
CLEAN_FILES += $(SCRIPT_PROG)

#
# The stat2resid script is configured with a different
# path to the supporting perl scripts, depending on whether it
# is to be installed or not.
#
ifeq "$(INSTALLING)" "1"
SCRIPT_PATH:=$(libdir)
else
SCRIPT_PATH:=$(FPTOOLS_TOP_ABS)/ghc/utils/stat2resid
endif

#
# install setup
# 
INSTALL_LIBS  += $(DYN_LOADABLE_BITS)
INSTALL_PROGS += $(SCRIPT_PROG)


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
