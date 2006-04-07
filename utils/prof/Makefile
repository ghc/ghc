TOP=../..
include $(TOP)/mk/boilerplate.mk

INSTALLING=1

ifeq "$(INSTALLING)" "1"
SUBDIRS = cgprof icons
endif

SCRIPT_SUBST_VARS= \
 FPTOOLS_TOP_ABS \
 INSTALLING \
 DEFAULT_TMPDIR \
 TARGETPLATFORM

INSTALLED_SCRIPT_PROG  = ghcprof
INPLACE_SCRIPT_PROG    = ghcprof-inplace

ifeq "$(INSTALLING)" "1"
SCRIPT_PROG 	=  $(INSTALLED_SCRIPT_PROG)
else
SCRIPT_PROG 	=  $(INPLACE_SCRIPT_PROG)
endif

ifneq "$(BIN_DIST)" "1"
SCRIPT_SUBST_VARS += libdir libexecdir
endif

# don't recurse on 'make install'
#
ifeq "$(INSTALLING)" "1"
all clean distclean mostlyclean maintainer-clean ::
	$(MAKE) INSTALLING=0 BIN_DIST=0 $(MFLAGS) $@
endif

INTERP          = perl
SCRIPT_OBJS	= ghcprof.prl
INSTALL_SCRIPTS += $(SCRIPT_PROG)

include $(TOP)/mk/target.mk
