TOP = ..
include $(TOP)/mk/boilerplate.mk

SUBDIRS = building users_guide ext-core storage-mgt

PAGES = index.html

install-docs :: $(PAGES)
	$(INSTALL_DIR) $(datadir)/html
	$(INSTALL_DATA) $(INSTALL_OPTS) $(PAGES) $(datadir)/html

include $(TOP)/mk/target.mk
