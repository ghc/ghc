#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.56 2001/03/19 18:15:59 sewardj Exp $
#

TOP=..
CURRENT_DIR=ghc/driver
include $(TOP)/mk/boilerplate.mk

# hack for ghci-inplace script, see below
INSTALLING=1

ifeq "$(INSTALLING)" "1"	
SUBDIRS = mangler split
endif

# -----------------------------------------------------------------------------
# Create compiler configuration

CURRENT_DIR=ghc/compiler
CONFIG_HS = Config.hs
boot :: $(CONFIG_HS)

$(CONFIG_HS) : $(FPTOOLS_TOP)/mk/config.mk Makefile
	@$(RM) -f $(CONFIG_HS)
	@echo -n "Creating $(CONFIG_HS) ... "
	@echo "module Config where" >>$(CONFIG_HS)
	@echo "cTARGETPLATFORM       = \"$(TARGETPLATFORM)\"" >> $(CONFIG_HS)
	@echo "cCURRENT_DIR          = \"$(CURRENT_DIR)\"" >> $(CONFIG_HS)
	@echo "cHaveLibGmp           = \"$(HaveLibGmp)\"" >> $(CONFIG_HS)
	@echo "cLibsReadline         = \"$(LibsReadline)\"" >> Config.hs
	@echo "clibdir               = \"$(libdir)\"" >> $(CONFIG_HS)
	@echo "cGHC_LIB_DIR          = \"$(GHC_LIB_DIR)\"" >> $(CONFIG_HS)
	@echo "cGHC_RUNTIME_DIR      = \"$(GHC_RUNTIME_DIR)\"" >> $(CONFIG_HS)
	@echo "cGHC_UTILS_DIR        = \"$(GHC_UTILS_DIR)\"" >> $(CONFIG_HS)
	@echo "cGHC_INCLUDE_DIR      = \"$(GHC_INCLUDE_DIR)\"" >> $(CONFIG_HS)
	@echo "cFPTOOLS_TOP_ABS      = \"$(FPTOOLS_TOP_ABS)\"" >> $(CONFIG_HS)
	@echo done.

CLEAN_FILES += $(CONFIG_HS)

# -----------------------------------------------------------------------------
# package configuration files...

ghc_407_at_least = $(shell expr "$(GhcMinVersion)" \>= 7)
ifeq "$(ghc_407_at_least)" "1"
ifneq "$(mingw32_TARGET_OS)" "1"
SRC_HC_OPTS += -fglasgow-exts -cpp -package concurrent -package posix -package text
else
SRC_HC_OPTS += -fglasgow-exts -cpp -package concurrent -package text
endif
else
SRC_HC_OPTS += -fglasgow-exts -cpp -syslib concurrent -syslib posix -syslib misc
endif

SRC_HC_OPTS += -DWANT_PRETTY

all :: package.conf package.conf.inplace

pkgconf : Config.o Package.o PackageSrc.o Utils.o
	$(HC) $(HC_OPTS) $(LD_OPTS) Config.o Package.o PackageSrc.o Utils.o -o pkgconf

package.conf.inplace : pkgconf
	./pkgconf in-place >$@

package.conf : pkgconf
	./pkgconf install >$@

Package.o : ../utils/ghc-pkg/Package.hs

INSTALL_DATAS += package.conf

CLEAN_FILES += pkgconf package.conf.inplace package.conf

# -----------------------------------------------------------------------------
# ghci script

ifeq "$(INSTALLING)" "1"
ifeq "$(BIN_DIST)"   "1"
GHCBIN=$$\"\"bindir/ghc
else
GHCBIN=$(bindir)/ghc
endif # BIN_DIST
else
GHCBIN=$(FPTOOLS_TOP_ABS)/ghc/compiler/ghc
endif

INSTALLED_SCRIPT_PROG  = ghci-$(ProjectVersion)
INPLACE_SCRIPT_PROG    = ghci-inplace

SCRIPT_OBJS	  = ghci.sh
INTERP		  = $(SHELL)
SCRIPT_SUBST_VARS = GHCBIN TOPDIROPT
INSTALL_SCRIPTS  += $(SCRIPT_PROG) ghc5

ifeq "$(INSTALLING)" "1"
SCRIPT_PROG 	=  $(INSTALLED_SCRIPT_PROG)
TOPDIROPT	=  
LINK 		=  ghci
else
TOPDIROPT	=  -B$(FPTOOLS_TOP_ABS)
SCRIPT_PROG 	=  $(INPLACE_SCRIPT_PROG)
endif

# don't recurse on 'make install'
#
ifeq "$(INSTALLING)" "1"
all clean distclean maintainer-clean ::
	$(MAKE) INSTALLING=0 BIN_DIST=0 $(MFLAGS) $@
endif

# -----------------------------------------------------------------------------
# installation...

override datadir=$(libdir)
INSTALL_DATAS += ghc-usage.txt

ghc5 :
	rm -f ghc5
	echo "#!/bin/sh" > ghc5
	echo "# You (the user) need to set GHC_TOPDIR to make it work!" >> ghc5
	echo "GHC_TOPDIR=/path/to/root/of/ghc/installation/tree" >> ghc5
	echo "GHC_PLATFORM=$(TARGETPLATFORM)" >> ghc5
	cat ghc5.sh >> ghc5



# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
