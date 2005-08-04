TOP = ..
include $(TOP)/mk/boilerplate.mk

INSTALLING=1

ghc_ge_504 := $(shell if (test $(GhcCanonVersion) -ge 504); then echo YES; else echo NO; fi)
ghc_ge_603 := $(shell if (test $(GhcCanonVersion) -ge 603); then echo YES; else echo NO; fi)

SRC_HC_OPTS += -fglasgow-exts -cpp

# Use hierarchical libraries for GHC >= 5.04
ifeq "$(ghc_ge_504)" "YES"
SRC_HC_OPTS += -package network
# Monad transformers live in their own package for GHC >= 6.3
ifeq "$(ghc_ge_603)" "YES"
SRC_HC_OPTS += -package mtl
endif
else
SRC_HC_OPTS += -package data -package text -package util -package net
endif

ifeq "$(Windows)" "YES"
HS_PROG_EXT = .exe
else
HS_PROG_EXT = .bin
endif

HS_PROG = haddock$(HS_PROG_EXT)

# Why HOST platform, not TARGET platform??? Confusing...
ifeq "$(Windows)" "YES"
Main_HC_OPTS += -Dmingw32_HOST_OS=1
endif

HsParser_HC_OPTS      += -Onot
HaddockParse_HC_OPTS  += -Onot

# -----------------------------------------------------------------------------
# Create the Version.hs file

VERSION_HS = Version.hs
EXTRA_SRCS += $(VERSION_HS)

boot :: $(VERSION_HS)

Version.hs : $(TOP)/mk/version.mk
	@$(RM) -f $(VERSION_HS)
	@echo "Creating $(VERSION_HS) ... "
	@echo "module Version where" >>$(VERSION_HS)
	@echo "version = \"$(ProjectVersion)\"" >> $(VERSION_HS)

# -----------------------------------------------------------------------------

ifeq "$(INSTALLING)" "1"
ifeq "$(BIN_DIST)"   "1"
HADDOCKLIB=$$\"\"datadir/haddock
HADDOCKBIN=$$\"\"libexecdir/$(HS_PROG)
else
ifeq "$(TARGETPLATFORM)" "i386-unknown-cygwin32"
# --mixed is only provided by newer versions of cygpath (1.2 doesn't have it, for instance.)
#HADDOCKLIB=$(shell cygpath --mixed $(datadir))
HADDOCKLIB=$(shell cygpath -w $(datadir) | sed -e 's%\\%/%g')
else
HADDOCKLIB=$(datadir)
endif
HADDOCKBIN=$(libexecdir)/$(HS_PROG)
endif # BIN_DIST
else
ifeq "$(TARGETPLATFORM)" "i386-unknown-cygwin32"
# See above comment re: --mixed
#HADDOCKLIB=$(shell cygpath --mixed $(FPTOOLS_TOP_ABS)/haddock/html)
HADDOCKLIB=$(shell cygpath -w $(FPTOOLS_TOP_ABS)/haddock/html | sed -e 's%\\%/%g')
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
ifeq "$(Windows)" "NO"
LINK	 	=  haddock
endif
else
TOP_PWD 	:= $(FPTOOLS_TOP_ABS)
SCRIPT_PROG 	=  $(INPLACE_SCRIPT_PROG)
endif

SCRIPT_OBJS=haddock.sh

INTERP=$(SHELL)

SCRIPT_SUBST_VARS = HADDOCKLIB HADDOCKBIN

# The script isn't installed on MinGW; we run the binary directly
ifeq "$(Windows)" "NO"
INSTALL_SCRIPTS += $(SCRIPT_PROG)
endif

INSTALL_LIBEXECS = $(HS_PROG)

# don't recurse on 'make install'
#
ifeq "$(INSTALLING)" "1"
all clean distclean maintainer-clean ::
	$(MAKE) INSTALLING=0 BIN_DIST=0 $(MFLAGS) $@
endif

include $(TOP)/mk/target.mk
