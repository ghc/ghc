# -----------------------------------------------------------------------------

TOP=../..
include $(TOP)/mk/boilerplate.mk

# hack for ghci-inplace script, see below
INSTALLING=1

# -----------------------------------------------------------------------------
# ghc-pkg.bin

SRC_HC_OPTS += -cpp -Wall -fno-warn-name-shadowing -fno-warn-unused-matches

# This causes libghccompat.a to be used:
include $(GHC_LIB_COMPAT_DIR)/compat.mk

# This is required because libghccompat.a must be built with
# $(GhcHcOpts) because it is linked to the compiler, and hence
# we must also build with $(GhcHcOpts) here:
SRC_HC_OPTS += $(GhcHcOpts)

ifeq "$(ghc_ge_504)" "NO"
SRC_HC_OPTS +=  -package lang -package util -package text
endif

# On Windows, ghc-pkg is a standalone program
# ($bindir/ghc-pkg.exe), whereas on Unix it needs a wrapper script
# to pass the appropriate flag to the real binary
# ($libexecdir/ghc-pkg.bin) so that it can find package.conf.
ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
HS_PROG           = ghc-pkg.exe
INSTALL_PROGS    += $(HS_PROG)
else
HS_PROG           = ghc-pkg.bin
INSTALL_LIBEXECS += $(HS_PROG)
endif

# -----------------------------------------------------------------------------=
# Create the Version.hs file

VERSION_HS = Version.hs
EXTRA_SRCS += $(VERSION_HS)

boot :: $(VERSION_HS)

Version.hs : Makefile $(TOP)/mk/version.mk
	@$(RM) -f $(VERSION_HS)
	@echo "Creating $(VERSION_HS) ... "
	@echo "module Version where" >>$(VERSION_HS)
	@echo "version, targetOS, targetARCH :: String" >>$(VERSION_HS)
	@echo "version    = \"$(ProjectVersion)\"" >> $(VERSION_HS)
	@echo "targetOS   = \"$(TargetOS_CPP)\"" >> $(VERSION_HS)
	@echo "targetARCH = \"$(TargetArch_CPP)\"" >> $(VERSION_HS)

DIST_CLEAN_FILES += $(VERSION_HS)

# -----------------------------------------------------------------------------
# ghc-pkg script

ifeq "$(INSTALLING)" "1"
ifeq "$(BIN_DIST)"   "1"
GHCPKGBIN=$$\"\"libexecdir/$(HS_PROG)
PKGCONF=$$\"\"libdir/package.conf
else
GHCPKGBIN=$(libexecdir)/$(HS_PROG)
PKGCONF=$(libdir)/package.conf
endif # BIN_DIST
else
GHCPKGBIN=$(FPTOOLS_TOP_ABS)/ghc/utils/ghc-pkg/$(HS_PROG)
PKGCONF=$(FPTOOLS_TOP_ABS_PLATFORM)/ghc/driver/package.conf.inplace
endif

ifneq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
INSTALLED_SCRIPT_PROG  = ghc-pkg-$(ProjectVersion)
endif
INPLACE_SCRIPT_PROG    = ghc-pkg-inplace

SCRIPT_OBJS	  = ghc-pkg.sh
INTERP		  = $(SHELL)
SCRIPT_SUBST_VARS = GHCPKGBIN PKGCONFOPT
ifneq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
INSTALL_SCRIPTS  += $(SCRIPT_PROG)
endif
PKGCONFOPT 	  = --global-conf $(PKGCONF)

ifeq "$(INSTALLING)" "1"
SCRIPT_PROG 	=  $(INSTALLED_SCRIPT_PROG)
ifneq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
LINK 		=  ghc-pkg
endif
else
SCRIPT_PROG 	=  $(INPLACE_SCRIPT_PROG)
endif

# -----------------------------------------------------------------------------
# don't recurse on 'make install'
#
ifeq "$(INSTALLING)" "1"
all :: $(HS_PROG)
	$(MAKE) INSTALLING=0 BIN_DIST=0 $(MFLAGS) $@
clean distclean maintainer-clean ::
	$(MAKE) INSTALLING=0 BIN_DIST=0 $(MFLAGS) $@
endif

# ghc-pkg is needed to boot in ghc/rts and library dirs
# Do a recursive 'make all' after generating dependencies, because this
# will work with 'make -j'.
ifneq "$(BootingFromHc)" "YES"
boot :: depend
	$(MAKE) all
endif

include $(TOP)/mk/target.mk
