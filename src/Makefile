TOP = ..
include $(TOP)/mk/boilerplate.mk

INSTALLING=1

SRC_HC_OPTS += -package data -package text -package util -package net -fglasgow-exts -cpp
HS_PROG = haddock.bin

HsParser_HC_OPTS      += -Onot
HaddockParse_HC_OPTS  += -Onot
HaddockVersion_HC_OPTS = -DHADDOCK_VERSION=$(ProjectVersion)

HaddockVersion : ../mk/version.mk

ifeq "$(INSTALLING)" "1"
ifeq "$(BIN_DIST)"   "1"
HADDOCKLIB=$$\"\"libdir/haddock
HADDOCKBIN=$$\"\"libexecdir/$(HS_PROG)
else
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
HADDOCKLIB=$(shell cygpath --mixed $(libdir))
else
HADDOCKLIB=$(libdir)
endif # mingw
HADDOCKBIN=$(libexecdir)/$(HS_PROG)
endif # BIN_DIST
else
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
HADDOCKLIB=$(shell cygpath --mixed $(FPTOOLS_TOP_ABS)/haddock/html)
else
HADDOCKLIB=$(FPTOOLS_TOP_ABS)/haddock/html
endif
HADDOCKBIN=$(FPTOOLS_TOP_ABS)/haddock/src/$(HS_PROG)
endif # INSTALLING

HAPPY_OPTS += $(GHC_HAPPY_OPTS)

INSTALLED_SCRIPT_PROG  = haddock-$(ProjectVersion)
INPLACE_SCRIPT_PROG    = haddock-inplace

ifeq "$(INSTALLING)" "1"
TOP_PWD 	:= $(prefix)
SCRIPT_PROG 	=  $(INSTALLED_SCRIPT_PROG)
LINK	 	=  haddock
else
TOP_PWD 	:= $(FPTOOLS_TOP_ABS)
SCRIPT_PROG 	=  $(INPLACE_SCRIPT_PROG)
endif

SCRIPT_OBJS=haddock.sh

INTERP=$(SHELL)

SCRIPT_SUBST_VARS = HADDOCKLIB HADDOCKBIN

INSTALL_SCRIPTS += $(SCRIPT_PROG)
INSTALL_LIBEXECS = $(HS_PROG)

# don't recurse on 'make install'
#
ifeq "$(INSTALLING)" "1"
all clean distclean maintainer-clean ::
	$(MAKE) INSTALLING=0 BIN_DIST=0 $(MFLAGS) $@
endif

include $(TOP)/mk/target.mk
