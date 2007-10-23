TOP = ..
include $(TOP)/mk/boilerplate.mk

SUBDIRS = man docbook-cheat-sheet users_guide
ifneq "$(DOING_BIN_DIST)" "YES"
SUBDIRS += ext-core storage-mgt
endif

PAGES = index.html

install-docs :: $(PAGES)
	$(INSTALL_DIR) 							 $(DESTDIR)$(htmldir)
	$(INSTALL_DATA) $(INSTALL_OPTS) $(PAGES) $(DESTDIR)$(htmldir)

.PHONY: binary-dist binary-dist.doc.%

binary-dist: $(foreach SUBDIR,$(SUBDIRS),binary-dist.doc.$(SUBDIR))
	$(MKDIRHIER) $(BIN_DIST_DIR)/docs
	cp Makefile $(BIN_DIST_DIR)/docs/
	cp $(PAGES) $(BIN_DIST_DIR)/docs/

$(foreach SUBDIR,$(SUBDIRS),binary-dist.doc.$(SUBDIR)): \
binary-dist.doc.%:
	$(MAKE) -C $* binary-dist

include $(TOP)/mk/target.mk
