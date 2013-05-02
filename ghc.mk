
utils/haddock_USES_CABAL = YES
utils/haddock_PACKAGE = haddock
utils/haddock_CONFIGURE_OPTS = --flag in-ghc-tree
utils/haddock_dist_SHELL_WRAPPER = YES
utils/haddock_dist_INSTALL = YES
utils/haddock_dist_INSTALL_INPLACE = YES
utils/haddock_dist_INSTALL_SHELL_WRAPPER_NAME = haddock-ghc-$(ProjectVersion)
utils/haddock_dist_PROGNAME = haddock

ifeq "$(HADDOCK_DOCS)" "NO"
utils/haddock_dist_NOT_NEEDED = YES
endif

$(eval $(call build-prog,utils/haddock,dist,2))

ifneq "$(BINDIST)" "YES"

$(INPLACE_BIN)/$(utils/haddock_dist_PROG): $(INPLACE_LIB)/html $(INPLACE_LIB)/latex

$(INPLACE_LIB)/html:
	$(call removeTrees,$@)
	"$(CP)" -RL utils/haddock/resources/html $@

$(INPLACE_LIB)/latex:
	$(call removeTrees,$@)
	"$(CP)" -R utils/haddock/resources/latex $@

endif

utils/haddock_dist_MODULES += Paths_haddock

ifeq "$(HADDOCK_DOCS)" "YES"
install: install_utils/haddock_data
ifeq "$(Windows_Host)" "NO"
install: install_utils/haddock_link
endif
endif

.PHONY: install_utils/haddock_data
install_utils/haddock_data:
	$(foreach i,$(sort $(dir $(utils/haddock_dist_DATA_FILES))), \
	    $(call make-command,$(call INSTALL_DIR,"$(DESTDIR)$(ghclibdir)/$i")))
	$(foreach i,$(utils/haddock_dist_DATA_FILES), \
	    $(call make-command,$(call INSTALL_DATA,$(INSTALL_OPTS),utils/haddock/resources/$i,"$(DESTDIR)$(ghclibdir)/$(dir $i)")))

.PHONY: install_utils/haddock_link
install_utils/haddock_link:
	$(call removeFiles,"$(DESTDIR)$(bindir)/haddock")
	$(LN_S) $(utils/haddock_dist_INSTALL_SHELL_WRAPPER_NAME) "$(DESTDIR)$(bindir)/haddock"

BINDIST_EXTRAS += $(addprefix utils/haddock/resources/,$(utils/haddock_dist_DATA_FILES))

