#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.48 2000/12/11 10:18:51 simonmar Exp $
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

SUBDIRS = mangler split stats

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
