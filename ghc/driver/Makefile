#-----------------------------------------------------------------------------
#

TOP=..
CURRENT_DIR=ghc/driver
include $(TOP)/mk/boilerplate.mk

# hack for ghci-inplace script, see below
INSTALLING=1

ifeq "$(INSTALLING)" "1"	
SUBDIRS = mangler split ghc ghci
endif

# -----------------------------------------------------------------------------
# package configuration files...

PKGCONF_OPTS = "$(TARGETPLATFORM)"		\
	       "$(CURRENT_DIR)"			\
	       "$(HaveLibGmp)"			\
	       "$(LibsReadline)"		\
	       "$(GHC_LIB_DIR)"			\
	       "$(GHC_RUNTIME_DIR)"		\
	       "$(GHC_UTILS_DIR)"		\
	       "$(GHC_INCLUDE_DIR)"

SRC_HC_OPTS += -fglasgow-exts -cpp 

ghc_407_at_least = $(shell if (test $(GhcCanonVersion) -ge 4070); then echo YES; else echo NO; fi)

ifeq "$(ghc_407_at_least)" "YES"
SRC_HC_OPTS += -package concurrent -package text
ifneq "$(mingw32_TARGET_OS)" "1"
SRC_HC_OPTS += -package posix
endif
else
SRC_HC_OPTS += -syslib concurrent -syslib posix -syslib misc
endif

SRC_HC_OPTS += -DWANT_PRETTY
SRC_HC_OPTS += $(filter -D% -U%,$(GhcRtsCcOpts))

all :: package.conf package.conf.inplace

HS_OBJS = Package.o PackageSrc.o Utils.o
HS_PROG = pkgconf

package.conf.inplace : $(HS_PROG)
	./$(HS_PROG) in-place $(PKGCONF_OPTS) >$@

package.conf : pkgconf
	./$(HS_PROG) install $(PKGCONF_OPTS) >$@

Package.o : ../utils/ghc-pkg/Package.hs

override datadir = $(libdir)
INSTALL_DATAS += package.conf ghc-usage.txt

CLEAN_FILES += Main.hi pkgconf package.conf.inplace package.conf

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
