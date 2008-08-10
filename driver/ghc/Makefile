#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.6 2001/10/23 16:32:30 rrt Exp $
#

TOP=../..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------
# ghc script

ifneq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
LINK = ghc
LINK_TARGET = $(LINK)-$(ProjectVersion)
INSTALLED_SCRIPT=$(DESTDIR)$(bindir)/$(LINK_TARGET)
install::
	$(RM) -f $(INSTALLED_SCRIPT)
	echo "#!$(SHELL)"                                 >> $(INSTALLED_SCRIPT)
	echo "GHCBIN=$(libexecdir)/ghc-$(ProjectVersion)" >> $(INSTALLED_SCRIPT)
	echo "TOPDIROPT=-B$(libdir)"                      >> $(INSTALLED_SCRIPT)
	echo 'exec $$GHCBIN $$TOPDIROPT $${1+"$$@"}'      >> $(INSTALLED_SCRIPT)
	$(EXECUTABLE_FILE) $(INSTALLED_SCRIPT)
endif

# -----------------------------------------------------------------------------

include $(TOP)/mk/bindist.mk

include $(TOP)/mk/target.mk
