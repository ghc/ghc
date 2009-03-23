
utils/haddock_USES_CABAL = YES
utils/haddock_PACKAGE = haddock
utils/haddock_CONFIGURE_OPTS = --flag in-ghc-tree
# XXX This is a temporary hack:
utils/haddock_HC_OPTS += -Wwarn
utils/haddock_dist_SHELL_WRAPPER = YES
utils/haddock_dist_INSTALL_SHELL_WRAPPER = YES
utils/haddock_dist_PROG = haddock

$(INPLACE_BIN)/$(utils/haddock_dist_PROG): $(INPLACE_LIB)/html

$(INPLACE_LIB)/html:
	$(RM) -rf $@
	$(CP) -R utils/haddock/html $@

install: install_utils/haddock_html
.PHONY: install_utils/haddock_html
install_utils/haddock_html:
	$(RM) -rf $(DESTDIR)$(datadir)/html
	$(CP) -R utils/haddock/html $(DESTDIR)$(datadir)/html

$(eval $(call build-prog,utils/haddock,dist,2))

utils/haddock_dist_MODULES += Paths_haddock
