TOP = ..
include $(TOP)/mk/boilerplate.mk

SUBDIRS = man building docbook-cheat-sheet ext-core storage-mgt users_guide

PAGES = index.html

install-docs :: $(PAGES)
	$(INSTALL_DIR) $(datadir)/html
	$(INSTALL_DATA) $(INSTALL_OPTS) $(PAGES) $(datadir)/html

include $(TOP)/mk/target.mk
