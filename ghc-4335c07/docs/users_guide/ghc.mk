# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------


docs/users_guide_RST_SOURCES := $(wildcard docs/users_guide/*.rst)

$(eval $(call sphinx,docs/users_guide,users_guide))

html_docs/users_guide : docs/users_guide/images/prof_scc.svg

# man page
docs/users_guide_MAN_RST_SOURCES := docs/users_guide/ghc.rst

MAN_SECTION := 1
MAN_PAGES := docs/users_guide/build-man/ghc.1

ifneq "$(BINDIST)" "YES"
$(MAN_PAGES): $(docs/users_guide_MAN_RST_SOURCES)
	$(SPHINXBUILD) -b man -d docs/users_guide/.doctrees-man docs/users_guide docs/users_guide/build-man
endif

$(eval $(call clean-target,users-guide,manpage,docs/users_guide/.doctrees-man/ docs/users_guide/build-man/))

man : $(MAN_PAGES)

ifeq "$(BUILD_MAN)" "YES"
ifeq "$(phase)" "final"
$(eval $(call all-target,users_guide/man,$(MAN_PAGES)))
endif

INSTALL_MANPAGES += $(MAN_PAGES)

install: install_man

.PHONY: install_man
install_man: $(MAN_PAGES)
	$(INSTALL_DIR) "$(DESTDIR)$(mandir)"
	$(INSTALL_DIR) "$(DESTDIR)$(mandir)/man$(MAN_SECTION)"
	$(INSTALL_MAN) $(INSTALL_OPTS) $(MAN_PAGES) "$(DESTDIR)$(mandir)/man$(MAN_SECTION)"

endif
