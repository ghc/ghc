#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.40 2000/06/30 09:46:04 simonmar Exp $
#

TOP=..
CURRENT_DIR=ghc/driver
include $(TOP)/mk/boilerplate.mk

ifeq "$(GhcWithHscBuiltViaC)" "YES"
HC=$(GHC_INPLACE)
endif

ghc_406_at_least = $(shell expr "$(GhcMinVersion)" \>= 6)
ifeq "$(ghc_406_at_least)" "1"
SRC_HC_OPTS += -fglasgow-exts -cpp -package concurrent -package posix -package text
else
SRC_HC_OPTS += -fglasgow-exts -cpp -syslib concurrent -syslib posix -syslib misc
endif

HS_PROG = ghc-$(ProjectVersion)
HS_SRCS = Config.hs Package.hs Main.hs
MKDEPENDHS_SRCS = Config.hs Main.hs PackageSrc.hs
LINK = ghc

SUBDIRS = mangler split stats

# -----------------------------------------------------------------------------
# Create driver configuration

boot :: Config.hs

Config.hs : $(FPTOOLS_TOP)/mk/config.mk Makefile
	@$(RM) -f Config.hs
	@echo -n "Creating Config.hs ... "
	@echo "module Config where" >>Config.hs
	@echo "cProjectName          = \"$(ProjectName)\"" >> Config.hs            
	@echo "cProjectVersion       = \"$(ProjectVersion)\"" >> Config.hs         
	@echo "cProjectVersionInt    = \"$(ProjectVersionInt)\"" >> Config.hs      
	@echo "cProjectPatchLevel    = \"$(ProjectPatchLevel)\"" >> Config.hs      
	@echo "cHOSTPLATFORM         = \"$(HOSTPLATFORM)\"" >> Config.hs           
	@echo "cTARGETPLATFORM       = \"$(TARGETPLATFORM)\"" >> Config.hs         
	@echo "cCURRENT_DIR          = \"$(CURRENT_DIR)\"" >> Config.hs            
	@echo "cGHC_LIB_DIR          = \"$(GHC_LIB_DIR)\"" >> Config.hs            
	@echo "cGHC_RUNTIME_DIR      = \"$(GHC_RUNTIME_DIR)\"" >> Config.hs        
	@echo "cGHC_UTILS_DIR        = \"$(GHC_UTILS_DIR)\"" >> Config.hs          
	@echo "cGHC_INCLUDE_DIR      = \"$(GHC_INCLUDE_DIR)\"" >> Config.hs        
	@echo "cGHC_DRIVER_DIR       = \"$(GHC_DRIVER_DIR)\"" >> Config.hs        
	@echo "cGCC                  = \"$(WhatGccIsCalled)\"" >> Config.hs
	@echo "cGhcWithNativeCodeGen = \"$(GhcWithNativeCodeGen)\"" >> Config.hs   
	@echo "cLeadingUnderscore    = \"$(LeadingUnderscore)\"" >> Config.hs      
	@echo "cGHC_MKDEPENDHS       = \"$(GHC_MKDEPENDHS)\"" >> Config.hs
	@echo "cGHC_UNLIT            = \"$(GHC_UNLIT)\"" >> Config.hs              
	@echo "cGHC_HSC              = \"$(GHC_HSC)\"" >> Config.hs                
	@echo "cGHC_MANGLER          = \"$(GHC_MANGLER)\"" >> Config.hs
	@echo "cGHC_SPLIT            = \"$(GHC_SPLIT)\"" >> Config.hs
	@echo "cGHC_STATS            = \"$(GHC_STATS)\"" >> Config.hs
	@echo "cGHC_SYSMAN           = \"$(GHC_SYSMAN)\"" >> Config.hs             
	@echo "cEnableWin32DLLs      = \"$(EnableWin32DLLs)\"" >> Config.hs        
	@echo "cCP                   = \"$(CP)\"" >> Config.hs                     
	@echo "cRM                   = \"$(RM)\"" >> Config.hs                     
	@echo "cCONTEXT_DIFF         = \"$(CONTEXT_DIFF)\"" >> Config.hs           
	@echo "cHaveLibGmp           = \"$(HaveLibGmp)\"" >> Config.hs
	@echo "cGhcWithRegisterised  = \"$(GhcWithRegisterised)\"" >> Config.hs    
	@echo "cUSER_WAY_NAMES       = \"$(USER_WAY_NAMES)\"" >> Config.hs         
	@echo "cUSER_WAY_OPTS        = \"$(USER_WAY_OPTS)\"" >> Config.hs          
	@echo "clibdir               = \"$(libdir)\"" >> Config.hs                 
	@echo "clibexecdir           = \"$(libexecdir)\"" >> Config.hs             
	@echo "cdatadir              = \"$(datadir)\"" >> Config.hs                
	@echo "cbindir               = \"$(bindir)\"" >> Config.hs                 
	@echo "cTMPDIR               = \"$(TMPDIR)\"" >> Config.hs                 
	@echo "cFPTOOLS_TOP_ABS      = \"$(FPTOOLS_TOP_ABS)\"" >> Config.hs
	@echo "cRAWCPP               = \"$(RAWCPP)\"" >> Config.hs
	@echo done.

CLEAN_FILES += Config.hs

# -----------------------------------------------------------------------------
# create ghc-inplace, a convenient way to run ghc from the build tree...

all :: ghc-inplace

ghc-inplace : ghc
	@$(RM) $@
	echo '#!/bin/sh' >>$@
	echo exec $(FPTOOLS_TOP_ABS)/ghc/driver/ghc -B$(FPTOOLS_TOP_ABS) '"$$@"' >>$@
	chmod 755 $@

CLEAN_FILES += ghc-inplace

# -----------------------------------------------------------------------------
# package configuration files...

all :: package.conf package.conf.inplace

pkgconf : Config.o Package.o PackageSrc.o
	$(HC) $(HC_OPTS) $(LD_OPTS) Config.o Package.o PackageSrc.o -o pkgconf

package.conf.inplace : pkgconf
	./pkgconf in-place >$@

package.conf : pkgconf
	./pkgconf install >$@

INSTALL_DATAS += package.conf

CLEAN_FILES += pkgconf package.conf.inplace package.conf

# -----------------------------------------------------------------------------
# installation...

INSTALL_PROGS = ghc-$(ProjectVersion)

override datadir=$(libdir)
INSTALL_DATAS += ghc-usage.txt

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk

# we need the driver for generating dependencies...
boot :: all

# -----------------------------------------------------------------------------
# Create link to from ghc-x.xx to ghc...

all :: $(LINK)

$(LINK) : $(HS_PROG)
	@if ( $(PERL) -e '$$fn="$(LINK)"; exit ((! -f $$fn || -l $$fn) ? 0 : 1);' ); then \
	   echo "Creating a symbolic link from $(HS_PROG) to $(LINK)"; \
	   $(RM) $(LINK); \
	   $(LN_S) $(HS_PROG) $(LINK); \
	 else \
	   echo "Creating a symbolic link from $(HS_PROG) to $(LINK) failed: \`$(LINK)' already exists"; \
	   echo "Perhaps remove \`$(LINK)' manually?"; \
	   exit 1; \
	 fi;

CLEAN_FILES += $(LINK)

install ::
	@if ( $(PERL) -e '$$fn="$(bindir)/$(LINK)"; exit ((! -f $$fn || -l $$fn) ? 0 : 1);' ); then \
	   echo "Creating a symbol link from $(HS_PROG) to $(LINK) in $(bindir)"; \
	   $(RM) $(bindir)/$(LINK); \
	   $(LN_S) $(HS_PROG) $(bindir)/$(LINK); \
	 else \
	   echo "Creating a symbol link from $(HS_PROG) to $(LINK) in $(bindir) failed: \`$(bindir)/$(LINK)' already exists"; \
	   echo "Perhaps remove \`$(bindir)/$(LINK)' manually?"; \
	   exit 1; \
	 fi;
