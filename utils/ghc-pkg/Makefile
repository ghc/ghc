TOP=../..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------
# ghc-pkg.bin

# Beyond stage 1, honour any Mac OS X depolyment target options.  If we use 
# these options in stage 1 we get a linker error if the bootstrap compiler is
#  for a more recent OS version
ifeq "$(stage)" "2"
SRC_CC_OPTS += $(MACOSX_DEPLOYMENT_CC_OPTS)
SRC_HC_OPTS += $(addprefix -optc, $(MACOSX_DEPLOYMENT_CC_OPTS))
SRC_LD_OPTS += $(addprefix -optl, $(MACOSX_DEPLOYMENT_LD_OPTS))
endif

SRC_HC_OPTS += -cpp -Wall -fno-warn-name-shadowing -fno-warn-unused-matches

SRC_HC_OPTS += $(PACKAGE_CABAL)

ifeq "$(Windows)" "NO"
SRC_HC_OPTS += -package unix
endif

ifeq "$(ghc_ge_607)" "YES"
SRC_HC_OPTS += -package containers
endif

SRC_HC_OPTS += -package Cabal

# On Windows, ghc-pkg is a standalone program
# ($bindir/ghc-pkg.exe), whereas on Unix it needs a wrapper script
# to pass the appropriate flag to the real binary
# ($libexecdir/ghc-pkg.bin) so that it can find package.conf.
# on Windows, we need to take control of filename globbing ourselves
ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
HS_PROG           = ghc-pkg.exe
INSTALL_PROGS    += $(HS_PROG)
EXCLUDE_SRCS     += CRT_noglob.c
NOGLOB_O          = CRT_noglob.o
else

# We have two version: the inplace version compiled by the bootstrap compiler
# and the install version compiled by the stage 1 compiler.
# When creating or using bindists we always want the final binary, as built
# by the stage 1 compiler.
ifeq "$(DOING_BIN_DIST)" "YES"
HS_PROG           = ghc-pkg.bin
else
ifeq "$(stage)" "2"
HS_PROG           = ghc-pkg.bin
else
HS_PROG           = ghc-pkg-inplace.bin
endif
endif
INSTALL_LIBEXECS += $(HS_PROG)
NOGLOB_O	  =

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
# ghc-pkg and ghc-pkg-inplace scripts

# ghc-pkg-inplace used to be either a /bin/sh script, or a .bat script
# on Windows.  It is now a real binary, compiled from a tiny .hs
# file.  The problem with using scripts here was that the .bat script
# cannot be executed by /bin/sh on MSYS (it can on Cygwin), but the
# /bin/sh script cannot be executed by Cabal.  So we would have needed
# both.  A single binary is therefore simpler.

INPLACE_HS=ghc-pkg-inplace.hs
INPLACE_PROG=ghc-pkg-inplace
EXCLUDED_SRCS+=$(INPLACE_HS)

$(INPLACE_HS): Makefile $(FPTOOLS_TOP)/mk/config.mk $(NOGLOB_O)
	echo "import System.Cmd; import System.Environment; import System.Exit" > $@
	echo "main = do args <- getArgs; rawSystem \"$(FPTOOLS_TOP_ABS)/$(GHC_PKG_DIR_REL)/$(HS_PROG)\" (\"--global-conf\":\"$(FPTOOLS_TOP_ABS)/driver/package.conf.inplace\":args) >>= exitWith" >> $@

$(INPLACE_PROG): $(INPLACE_HS)
	$(HC) --make $< -o $@ $(LD_OPTS) $(NOGLOB_O)

all :: $(INPLACE_PROG)

CLEAN_FILES += $(INPLACE_HS) $(INPLACE_PROG)

ifneq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
LINK = ghc-pkg
LINK_TARGET = $(LINK)-$(ProjectVersion)
INSTALLED_SCRIPT=$(DESTDIR)$(bindir)/$(LINK_TARGET)
install::
	$(INSTALL_DIR) $(DESTDIR)$(bindir)
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

binary-dist:
	$(INSTALL_DIR)                $(BIN_DIST_DIR)/utils/ghc-pkg
	$(INSTALL_DATA)    Makefile   $(BIN_DIST_DIR)/utils/ghc-pkg/
	$(INSTALL_PROGRAM) $(HS_PROG) $(BIN_DIST_DIR)/utils/ghc-pkg/

include $(TOP)/mk/target.mk
