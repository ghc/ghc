TOP=../..
include $(TOP)/mk/boilerplate.mk
INSTALLING=0

WAYS=

boot :: all

SCRIPT_PROG=mkdependHS
SCRIPT_OBJS=mkdependHS.prl
SCRIPT_SUBST_VARS= \
 RAWCPP TMPDIR UNLIT_PROG \
 TOP_PWD \
 INSTALL_LIB_DIR INSTALL_DATA_DIR \
 INSTALL_HSLIBS_DIR INSTALLING \
 PRELUDE_DIRS \
 PROJECTVERSION SED

INTERP=perl

#
# install setup
#
INSTALL_PROGS += $(SCRIPT_PROG)

INSTALL_HSLIBS_DIR:=$(libdir)/../hslibs-$(ProjectVersion)
INSTALL_LIB_DIR:=$(libdir)
INSTALL_DATA_DIR:=$(datadir)

ifeq "$(INSTALLING)" "1"
TOP_PWD:=$(prefix)
UNLIT_PROG:=$(libexecdir)/unlit
PRELUDE_DIRS:=$(libdir)/imports
else
TOP_PWD:=$(FPTOOLS_TOP_ABS)
UNLIT_PROG:=$(FPTOOLS_TOP_ABS)/ghc/utils/unlit/unlit
PRELUDE_DIRS:="$(FPTOOLS_TOP_ABS)/ghc/lib/ghc:$(FPTOOLS_TOP_ABS)/ghc/lib/required:$(FPTOOLS_TOP_ABS)/ghc/lib/glaExts:$(FPTOOLS_TOP_ABS)/ghc/lib/concurrent"
endif

#
# Before really installing the script, we have to
# reconfigure it such that the paths it refers to,
# point to the installed utils.
#
install ::
	$(RM) $(SCRIPT_PROG)
	$(MAKE) $(MFLAGS) INSTALLING=1 $(SCRIPT_PROG)

CLEAN_FILES += $(SCRIPT_PROG)

include $(TOP)/mk/target.mk

# Hack to re-create the in-situ build tree script after 
# having just installed it.
#
install ::
	@$(RM) $(SCRIPT_PROG)
	@$(MAKE) $(MFLAGS) $(SCRIPT_PROG)
