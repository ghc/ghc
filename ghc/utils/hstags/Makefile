TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/version.mk

# Note: might be overridden from cmd-line (see install rule below)
INSTALLING=0

C_PROG=hstags-help
SRC_CC_OPTS += -O

SCRIPT_PROG=hstags
SCRIPT_OBJS=hstags.prl

SCRIPT_SUBST_VARS=\
 INSTALLING \
 TOP_PWD \
 ProjectVersionInt

ifneq "$(BIN_DIST)" "1"
SCRIPT_SUBST_VARS += libdir libexecdir DEFAULT_TMPDIR
endif

#
# The hstags script is configured with different
# set of config variables, depending on whether it
# is to be installed or not.
#
ifeq "$(INSTALLING)" "1"
TOP_PWD := $(prefix)
ifeq "$(BIN_DIST)" "1"
SCRIPT_PREFIX_FILES += prefix.txt
endif
else
TOP_PWD := $(FPTOOLS_TOP_ABS)
HSP_IMPORTS:="$(TOP_PWD)/ghc/lib/ghc":"$(TOP_PWD)/ghc/lib/required":"$(TOP_PWD)/ghc/lib/glaExts":"$(TOP_PWD)/ghc/lib/concurrent"
SCRIPT_SUBST_VARS += HSP_IMPORTS
endif

#
# no INTERP: do *not* want #! script stuck on the front
#
# what's the deal? I'll add it for now (and perhaps pay for it later :-)
#  -- SOF
INTERP=perl

#
# install setup
#
INSTALL_SCRIPTS+=$(SCRIPT_PROG)
INSTALL_LIBEXECS=$(C_PROG)

#
# Before really installing the script, we have to
# reconfigure it such that the paths it refers to,
# point to the installed utils.
#
install ::
	@$(RM) $(SCRIPT_PROG)
	@$(MAKE) $(MFLAGS) INSTALLING=1 $(SCRIPT_PROG)

include $(TOP)/mk/target.mk


# Hack to re-create the in-situ build tree script after 
# having just installed it.
#
install ::
	@$(RM) $(SCRIPT_PROG)
	@$(MAKE) $(MFLAGS) BIN_DIST=0 $(SCRIPT_PROG)

