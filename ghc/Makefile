#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.3 1997/03/14 07:53:55 simonpj Exp $
#

TOP=.
include $(TOP)/mk/boilerplate.mk

line = @echo "------------------------------------------------------------------------------"

#
# subdir dependencies:
#	everything needs utils
#	includes/ needs driver (to easily c.pile mkNativeGen.c)
#	make depend except in {utils,driver} needs includes	
#	RTS and compiler need includes
#

boot ::
	$(line)
	@echo "Booting Utils"
	$(line)
	@$(MAKE) -C utils boot depend

	$(line)
	@echo "Booting Driver"
	$(line)
	@$(MAKE) -C driver boot depend

	$(line)
	@echo "Booting Includes"
	$(line)
	@$(MAKE) -C includes boot depend

	$(line)
	@echo "Booting Runtime System"
	$(line)
	@$(MAKE) -C runtime boot depend

	$(line)
	@echo "Booting Docs"
	$(line)
	@$(MAKE) -C docs boot

	$(line)
	@echo "Booting Compiler"
	$(line)
	@$(MAKE) -C compiler boot

	$(line)
	@echo "Booting Prelude libraries"
	$(line)
	@$(MAKE) -C compiler boot


# "CONTRIB" is also a SUBDIR, but there is nothing to build there.
# 
# leave out docs for the moment -- SOF
#
# Order is important! driver/ has to come before includes/ which
# again has to come before the rest.
#
# If we're booting from .hc files, swap the order
# we descend into compiler/ and lib/
#
ifeq "$(GhcWithHscBuiltViaC)" "NO"
SUBDIRS = utils driver includes runtime compiler lib
else
SUBDIRS = utils driver includes runtime lib compiler
endif

include $(TOP)/mk/target.mk
