TOP = ..
include $(TOP)/mk/boilerplate.mk

SUBDIRS = users_guide

install-docs :: index.html
	$(INSTALL_DATA) $(INSTALL_OPTS) index.html $(datadir)/html

include $(TOP)/mk/target.mk
