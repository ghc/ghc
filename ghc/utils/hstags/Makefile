TOP=../..
include $(TOP)/mk/boilerplate.mk

# No ways
WAYS=

# Note: might be overridden from cmd-line (see install rule below)
INSTALLING=0

C_PROG=hstags-help

SCRIPT_PROG=hstags
SCRIPT_OBJS=hstags.prl

SCRIPT_SUBST_VARS=\
 INSTALLING \
 TMPDIR \
 TOP_PWD \
 PROJECTVERSION

ifneq "$(BIN_DIST)" "1"
SCRIPT_SUBST_VARS += INSTLIBDIR_GHC
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
else
INSTLIBDIR_GHC=$(libdir)
endif
else
TOP_PWD := $(FPTOOLS_TOP_ABS)
INSTLIBDIR_GHC=$(libdir)
HSP_IMPORTS:="$(TOP_PWD)/ghc/lib/ghc":"$(TOP_PWD)/ghc/lib/required":"$(TOP_PWD)/ghc/lib/glaExts":"$(TOP_PWD)/ghc/lib/concurrent"
SCRIPT_SUBST_VARS += HSP_IMPORTS
endif

#
# no INTERP: do *not* want #! script stuck on the front
#
# what's the deal? I'll add it for now (and perhaps pay for it later :-)
#  -- SOF
INTERP=$(PERL)

#
# install setup
#
INSTALL_PROGS=$(SCRIPT_PROG)
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

