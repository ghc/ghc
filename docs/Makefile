TOP = ..
include $(TOP)/mk/boilerplate.mk

SUBDIRS = man docbook-cheat-sheet ext-core storage-mgt users_guide

PAGES = index.html

install-docs :: $(PAGES)
	$(INSTALL_DIR) 							 $(DESTDIR)$(htmldir)
	$(INSTALL_DATA) $(INSTALL_OPTS) $(PAGES) $(DESTDIR)$(htmldir)

include $(TOP)/mk/target.mk
