#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.50 2000/12/20 10:42:54 simonmar Exp $
#

TOP=..
CURRENT_DIR=ghc/driver
include $(TOP)/mk/boilerplate.mk

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

SUBDIRS = mangler split

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
# installation...

override datadir=$(libdir)
INSTALL_DATAS += ghc-usage.txt

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
