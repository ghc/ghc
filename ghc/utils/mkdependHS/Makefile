TOP=../..
include $(TOP)/mk/boilerplate.mk
INSTALLING=0

boot :: all

SCRIPT_PROG=mkdependHS
SCRIPT_OBJS=mkdependHS.prl
SCRIPT_SUBST_VARS= \
 TMPDIR \
 TOP_PWD \
 INSTALLING \
 PROJECTVERSION

INTERP=perl

#
# install setup
#
INSTALL_SCRIPTS += $(SCRIPT_PROG)

ifneq "$(BIN_DIST)" "1"
SCRIPT_SUBST_VARS += libdir datadir RAWCPP SED
endif


#
# Prepend an infomercial if installing (binary-dist or the real thing).
#
ifeq "$(INSTALLING)" "1"
ifeq "$(BIN_DIST)" "1"
SCRIPT_PREFIX_FILES += prefix.txt
endif
endif

ifeq "$(INSTALLING)" "1"
TOP_PWD:=$(prefix)
else
TOP_PWD:=$(FPTOOLS_TOP_ABS)
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

#
# Hack to re-create the in-situ build tree script after 
# having just installed it (if you don't, confusion ensues
# if you try to use the build tree copy afterwards..)
#
install ::
	@$(RM) $(SCRIPT_PROG)
	@$(MAKE) $(MFLAGS) $(SCRIPT_PROG)
