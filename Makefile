#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.2 1996/11/21 16:45:54 simonm Exp $

TOP = .
SUBDIRS = glafp-utils ghc
include $(TOP)/mk/gen.mk
include $(TOP)/mk/subdir.mk

line = @echo "------------------------------------------------------------------------------"

boot ::
	@echo "Bootstrapping $(PROJECTNAME)..."
	$(line)
	@echo "Booting glafp-utils"
	$(line)
	@$(MAKE) -C glafp-utils boot

	$(line)
	@echo "Booting ghc"
	$(line)
	@$(MAKE) -C ghc boot

	@echo "Done!"
