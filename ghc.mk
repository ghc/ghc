
utils/haddock_USES_CABAL = YES
utils/haddock_PACKAGE = haddock
utils/haddock_CONFIGURE_OPTS = --flag in-ghc-tree
utils/haddock_HC_OPTS += -DNEW_GHC_LAYOUT
utils/haddock_dist_SHELL_WRAPPER = YES
utils/haddock_dist_INSTALL_SHELL_WRAPPER = YES
utils/haddock_dist_INSTALL_SHELL_WRAPPER_NAME = haddock-$(ProjectVersion)
utils/haddock_dist_PROG = haddock$(exeext)

ifneq "$(BINDIST)" "YES"

$(INPLACE_BIN)/$(utils/haddock_dist_PROG): $(INPLACE_LIB)/html $(INPLACE_LIB)/latex

$(INPLACE_LIB)/html:
	"$(RM)" $(RM_OPTS_REC) $@
	"$(CP)" -R utils/haddock/html $@

$(INPLACE_LIB)/latex:
	"$(RM)" $(RM_OPTS_REC) $@
	"$(CP)" -R utils/haddock/latex $@

endif

$(eval $(call build-prog,utils/haddock,dist,2))

utils/haddock_dist_MODULES += Paths_haddock

install: install_utils/haddock_html
.PHONY: install_utils/haddock_html
install_utils/haddock_html:
	$(INSTALL_DIR) "$(DESTDIR)$(docdir)/html"
	"$(CP)" -R utils/haddock/html "$(DESTDIR)$(docdir)/html"

install: install_utils/haddock_data
.PHONY: install_utils/haddock_data
install_utils/haddock_data:
	$(INSTALL_DIR) "$(DESTDIR)$(ghclibdir)/html"
	for i in utils/haddock/html/*; do \
	    $(INSTALL_DATA) $(INSTALL_OPTS) $$i "$(DESTDIR)$(ghclibdir)/html"; \
	done

ifeq "$(Windows)" "NO"
install: install_haddock_link
.PNONY: install_haddock_link
install_haddock_link:
	"$(RM)" $(RM_OPTS) "$(DESTDIR)$(bindir)/haddock"
	$(LN_S) haddock-$(ProjectVersion) "$(DESTDIR)$(bindir)/haddock"
endif

BINDIST_EXTRAS += $(addprefix utils/haddock/,html/*)

