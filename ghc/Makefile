#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.2 1996/11/21 16:46:26 simonm Exp $

TOP=..
include $(TOP)/ghc/mk/ghc.mk

line = @echo "------------------------------------------------------------------------------"

define BuildLibs
$(line)
@echo "Building Libraries"
$(line)
@$(MAKE) -C lib depend all
endef

define BuildCompiler
$(line)
@echo "Building Compiler"
$(line)
@$(MAKE) -C compiler depend all
endef

# dependencies:
#	everything needs utils
#	includes needs driver (for mkNativeGen.c)
#	make depend except in {utils,driver} needs includes	
#	RTS and compiler need includes

boot ::
	$(line)
	@echo "Building Utils"
	$(line)
	@$(MAKE) -C utils depend all

	$(line)
	@echo "Building Driver"
	$(line)
	@$(MAKE) -C driver all

	$(line)
	@echo "Building Includes"
	$(line)
	@$(MAKE) -C includes all

	$(line)
	@echo "Building Runtime System"
	$(line)
	@$(MAKE) -C runtime depend all

	$(line)
	@echo "Building Docs"
	$(line)
	@$(MAKE) -C docs depend all

ifeq ($(HaskellCompilerType), HC_USE_HC_FILES)
	$(BuildLibs)
	$(BuildCompiler)
else
	$(BuildCompiler)
	$(BuildLibs)
endif

# "CONTRIB" is also a SUBDIR, but there is nothing to build there.
SUBDIRS = utils driver includes runtime docs compiler lib

# Make the required directories for install.

install_dirs ::
	$(MKDIRHIER) $(INSTBINDIR_GHC)
	$(MKDIRHIER) $(INSTSCRIPTDIR_GHC)
	$(MKDIRHIER) $(INSTLIBDIR_GHC)
	$(MKDIRHIER) $(INSTLIBDIR_GHC)/includes
	$(MKDIRHIER) $(INSTDATADIR_GHC)
	$(MKDIRHIER) $(INSTDATADIR_GHC)/includes
	$(MKDIRHIER) $(INSTDATADIR_GHC)/imports

install :: install_dirs

include $(TOP)/mk/subdir.mk
