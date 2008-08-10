TOP = ..
include $(TOP)/mk/boilerplate.mk

SUBDIRS = man docbook-cheat-sheet users_guide
ifneq "$(DOING_BIN_DIST)" "YES"
SUBDIRS += ext-core storage-mgt
endif

PAGE = index.html

install-docs :: $(PAGE)
	$(INSTALL_DIR)                          $(DESTDIR)$(htmldir)
	$(INSTALL_DATA) $(INSTALL_OPTS) $(PAGE) $(DESTDIR)$(htmldir)

.PHONY: binary-dist binary-dist.doc.%

binary-dist: $(foreach SUBDIR,$(SUBDIRS),binary-dist.doc.$(SUBDIR))
	echo $(WHERE_AM_I)/Makefile >> $(BIN_DIST_LIST)
	echo $(WHERE_AM_I)/$(PAGE)  >> $(BIN_DIST_LIST)

$(foreach SUBDIR,$(SUBDIRS),binary-dist.doc.$(SUBDIR)): \
binary-dist.doc.%:
	$(MAKE) -C $* binary-dist WHERE_AM_I=$(WHERE_AM_I)/$*

include $(TOP)/mk/target.mk
