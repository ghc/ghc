
utils/haddock_USES_CABAL = YES
utils/haddock_PACKAGE = haddock
utils/haddock_CONFIGURE_OPTS = --flag in-ghc-tree
# XXX This is a temporary hack:
utils/haddock_HC_OPTS += -Wwarn -DNEW_GHC_LAYOUT
utils/haddock_dist_SHELL_WRAPPER = YES
utils/haddock_dist_INSTALL_SHELL_WRAPPER = YES
utils/haddock_dist_PROG = haddock$(exeext)

ifneq "$(BINDIST)" "YES"

$(INPLACE_BIN)/$(utils/haddock_dist_PROG): $(INPLACE_LIB)/html

$(INPLACE_LIB)/html:
	"$(RM)" $(RM_OPTS) -r $@
	"$(CP)" -R utils/haddock/html $@

endif

$(eval $(call build-prog,utils/haddock,dist,2))

utils/haddock_dist_MODULES += Paths_haddock

install: install_utils/haddock_html
.PHONY: install_utils/haddock_html
install_utils/haddock_html:
	"$(RM)" $(RM_OPTS) -r $(DESTDIR)$(datadir)/html
	"$(CP)" -R utils/haddock/html $(DESTDIR)$(datadir)/html

BINDIST_EXTRAS += $(addprefix utils/haddock/,html/*)

