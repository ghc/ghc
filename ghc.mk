
utils/haddock_USES_CABAL = YES
utils/haddock_PACKAGE = haddock
utils/haddock_CONFIGURE_OPTS = --flag in-ghc-tree
utils/haddock_dist_SHELL_WRAPPER = YES
utils/haddock_dist_INSTALL_SHELL_WRAPPER = YES
utils/haddock_dist_INSTALL_SHELL_WRAPPER_NAME = haddock-ghc-$(ProjectVersion)
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

ifeq "$(HADDOCK_DOCS)" "NO"
utils/haddock_dist_NOT_NEEDED = YES
endif

$(eval $(call build-prog,utils/haddock,dist,2))

utils/haddock_dist_MODULES += Paths_haddock

ifeq "$(HADDOCK_DOCS)" "YES"
install: install_utils/haddock_data
ifeq "$(Windows)" "NO"
install: install_utils/haddock_link
endif
endif

.PHONY: install_utils/haddock_data
install_utils/haddock_data:
	$(foreach i,$(sort $(dir $(utils/haddock_dist_DATA_FILES))), \
	    $(call make-command,$(call INSTALL_DIR,"$(DESTDIR)$(ghclibdir)/$i")))
	$(foreach i,$(utils/haddock_dist_DATA_FILES), \
	    $(call make-command,$(call INSTALL_DATA,$(INSTALL_OPTS),utils/haddock/$i,"$(DESTDIR)$(ghclibdir)/$(dir $i)")))

.PHONY: install_utils/haddock_link
install_utils/haddock_link:
	"$(RM)" $(RM_OPTS) "$(DESTDIR)$(bindir)/haddock"
	$(LN_S) $(utils/haddock_dist_INSTALL_SHELL_WRAPPER_NAME) "$(DESTDIR)$(bindir)/haddock"

BINDIST_EXTRAS += $(addprefix utils/haddock/,$(utils/haddock_dist_DATA_FILES))

