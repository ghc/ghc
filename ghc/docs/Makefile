TOP = ..
include $(TOP)/mk/boilerplate.mk

#
# No ways and export this piece of config info downwards.
#
export WAYS=

SUBDIRS = users_guide

install-docs :: index.html
	$(INSTALL_DATA) $(INSTALL_OPTS) index.html $(datadir)/html

include $(TOP)/mk/target.mk
