#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.51 2001/02/11 16:32:48 simonmar Exp $
#

TOP=..
CURRENT_DIR=ghc/driver
include $(TOP)/mk/boilerplate.mk

# hack for ghci-inplace script, see below
INSTALLING=1

ifeq "$(GhcWithHscBuiltViaC)" "YES"
HC=$(GHC_INPLACE)
endif

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

all :: package.conf package.conf.inplace

pkgconf : Config.o Package.o PackageSrc.o Utils.o
	$(HC) $(HC_OPTS) $(LD_OPTS) Config.o Package.o PackageSrc.o Utils.o -o pkgconf

package.conf.inplace : pkgconf
	./pkgconf in-place >$@

package.conf : pkgconf
	./pkgconf install >$@

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

ifeq "$(INSTALLING)" "1"
TOP_PWD 	:= $(prefix)
SCRIPT_PROG 	=  $(INSTALLED_SCRIPT_PROG)
SCRIPT_LINK 	=  ghci
else
TOP_PWD 	:= $(FPTOOLS_TOP_ABS)
SCRIPT_PROG 	=  $(INPLACE_SCRIPT_PROG)
endif

SCRIPT_OBJS=ghci.sh

INTERP=$(SHELL)

SCRIPT_SUBST_VARS = GHCBIN

INSTALL_SCRIPTS += $(SCRIPT_PROG)

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

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
