TOP=../..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------
# ghc-pkg.bin

SRC_HC_OPTS += -cpp -Wall -fno-warn-name-shadowing -fno-warn-unused-matches

# This causes libghccompat.a to be used:
include $(GHC_COMPAT_DIR)/compat.mk

SRC_HC_OPTS += $(PACKAGE_CABAL)

# This is required because libghccompat.a must be built with
# $(GhcHcOpts) because it is linked to the compiler, and hence
# we must also build with $(GhcHcOpts) here:
SRC_HC_OPTS += $(GhcHcOpts) $(GhcStage1HcOpts)

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

# -----------------------------------------------------------------------------
# Create the Version.hs file

VERSION_HS = Version.hs
EXTRA_SRCS += $(VERSION_HS)

boot :: $(VERSION_HS)

Version.hs : Makefile $(TOP)/mk/config.mk
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

ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
# See "MSys Note 3" in compiler/Makefile
INPLACE_SCRIPT=ghc-pkg-inplace.bat
all::
	$(RM) -f $(INPLACE_SCRIPT)
	echo '@call $(subst /,\,$(FPTOOLS_TOP_ABS_PLATFORM)/utils/ghc-pkg/$(HS_PROG)) --global-conf $(FPTOOLS_TOP_ABS_PLATFORM)/driver/package.conf.inplace %*' >> $(INPLACE_SCRIPT)
	$(EXECUTABLE_FILE) $(INPLACE_SCRIPT)
else
INPLACE_SCRIPT=ghc-pkg-inplace
all::
	$(RM) -f $(INPLACE_SCRIPT)
	echo "#!$(SHELL)"                                                      >> $(INPLACE_SCRIPT)
	echo "GHCPKGBIN=$(FPTOOLS_TOP_ABS)/utils/ghc-pkg/$(HS_PROG)"           >> $(INPLACE_SCRIPT)
	echo "PKGCONF=$(FPTOOLS_TOP_ABS_PLATFORM)/driver/package.conf.inplace" >> $(INPLACE_SCRIPT)
	echo 'exec $$GHCPKGBIN --global-conf $$PKGCONF $${1+"$$@"}'            >> $(INPLACE_SCRIPT)
	$(EXECUTABLE_FILE) $(INPLACE_SCRIPT)
endif
CLEAN_FILES += $(INPLACE_SCRIPT)

ifneq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
LINK = ghc-pkg
LINK_TARGET = $(LINK)-$(ProjectVersion)
INSTALLED_SCRIPT=$(bindir)/$(LINK_TARGET)
install::
	$(RM) -f $(INSTALLED_SCRIPT)
	echo "#!$(SHELL)"                                           >> $(INSTALLED_SCRIPT)
	echo "GHCPKGBIN=$(libexecdir)/$(HS_PROG)"                   >> $(INSTALLED_SCRIPT)
	echo "PKGCONF=$(libdir)/package.conf"                       >> $(INSTALLED_SCRIPT)
	echo 'exec $$GHCPKGBIN --global-conf $$PKGCONF $${1+"$$@"}' >> $(INSTALLED_SCRIPT)
	$(EXECUTABLE_FILE) $(INSTALLED_SCRIPT)
endif

# ghc-pkg is needed to boot in rts/ and library dirs
# Do a recursive 'make all' after generating dependencies, because this
# will work with 'make -j'.
ifneq "$(BootingFromHc)" "YES"
boot :: depend
	$(MAKE) all
endif

include $(TOP)/mk/target.mk
