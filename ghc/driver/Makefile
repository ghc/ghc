#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.32 2000/06/13 16:07:20 simonmar Exp $
#

TOP=..
CURRENT_DIR=ghc/driver
include $(TOP)/mk/boilerplate.mk

ifneq "$(GhcWithHscBuiltViaC)" "YES"
HC=$(WithGhcHc)
else
HC=$(GHC_INPLACE)
endif

SRC_HC_OPTS += -fglasgow-exts -cpp -syslib concurrent -syslib posix -syslib text

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
	@echo "_ProjectName          = \"$(ProjectName)\"" >> Config.hs            
	@echo "_ProjectVersion       = \"$(ProjectVersion)\"" >> Config.hs         
	@echo "_ProjectVersionInt    = \"$(ProjectVersionInt)\"" >> Config.hs      
	@echo "_ProjectPatchLevel    = \"$(ProjectPatchLevel)\"" >> Config.hs      
	@echo "_HOSTPLATFORM         = \"$(HOSTPLATFORM)\"" >> Config.hs           
	@echo "_TARGETPLATFORM       = \"$(TARGETPLATFORM)\"" >> Config.hs         
	@echo "_CURRENT_DIR          = \"$(CURRENT_DIR)\"" >> Config.hs            
	@echo "_GHC_LIB_DIR          = \"$(GHC_LIB_DIR)\"" >> Config.hs            
	@echo "_GHC_RUNTIME_DIR      = \"$(GHC_RUNTIME_DIR)\"" >> Config.hs        
	@echo "_GHC_UTILS_DIR        = \"$(GHC_UTILS_DIR)\"" >> Config.hs          
	@echo "_GHC_INCLUDE_DIR      = \"$(GHC_INCLUDE_DIR)\"" >> Config.hs        
	@echo "_GHC_DRIVER_DIR       = \"$(GHC_DRIVER_DIR)\"" >> Config.hs        
	@echo "_GCC                  = \"$(WhatGccIsCalled)\"" >> Config.hs
	@echo "_GhcWithNativeCodeGen = \"$(GhcWithNativeCodeGen)\"" >> Config.hs   
	@echo "_LeadingUnderscore    = \"$(LeadingUnderscore)\"" >> Config.hs      
	@echo "_GHC_MKDEPENDHS       = \"$(GHC_MKDEPENDHS)\"" >> Config.hs
	@echo "_GHC_UNLIT            = \"$(GHC_UNLIT)\"" >> Config.hs              
	@echo "_GHC_HSCPP            = \"$(GHC_HSCPP)\"" >> Config.hs              
	@echo "_GHC_HSC              = \"$(GHC_HSC)\"" >> Config.hs                
	@echo "_GHC_MANGLER          = \"$(GHC_MANGLER)\"" >> Config.hs
	@echo "_GHC_SPLIT            = \"$(GHC_SPLIT)\"" >> Config.hs
	@echo "_GHC_STATS            = \"$(GHC_STATS)\"" >> Config.hs
	@echo "_GHC_SYSMAN           = \"$(GHC_SYSMAN)\"" >> Config.hs             
	@echo "_EnableWin32DLLs      = \"$(EnableWin32DLLs)\"" >> Config.hs        
	@echo "_CP                   = \"$(CP)\"" >> Config.hs                     
	@echo "_RM                   = \"$(RM)\"" >> Config.hs                     
	@echo "_CONTEXT_DIFF         = \"$(CONTEXT_DIFF)\"" >> Config.hs           
	@echo "_HaveLibGmp           = \"$(HaveLibGmp)\"" >> Config.hs
	@echo "_GhcWithRegisterised  = \"$(GhcWithRegisterised)\"" >> Config.hs    
	@echo "_USER_WAY_NAMES       = \"$(USER_WAY_NAMES)\"" >> Config.hs         
	@echo "_USER_WAY_OPTS        = \"$(USER_WAY_OPTS)\"" >> Config.hs          
	@echo "_libdir               = \"$(libdir)\"" >> Config.hs                 
	@echo "_libexecdir           = \"$(libexecdir)\"" >> Config.hs             
	@echo "_datadir              = \"$(datadir)\"" >> Config.hs                
	@echo "_bindir               = \"$(bindir)\"" >> Config.hs                 
	@echo "_TMPDIR               = \"$(TMPDIR)\"" >> Config.hs                 
	@echo "_FPTOOLS_TOP_ABS      = \"$(FPTOOLS_TOP_ABS)\"" >> Config.hs
	@echo done.

CLEAN_FILES += Config.hs

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

# -----------------------------------------------------------------------------
# create ghc-inplace, a convenient way to run ghc from the build tree...

all :: ghc-inplace

ghc-inplace : ghc
	@$(RM) $@
	echo '#!/bin/sh' >>$@
	echo exec $(FPTOOLS_TOP_ABS)/ghc/driver/ghc -B$(FPTOOLS_TOP_ABS) $$\* >>$@
	chmod 755 $@

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

INSTALL_PROGS = ghc

override datadir=$(libdir)
INSTALL_DATAS += ghc-usage.txt

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
