utils/haddock_USES_CABAL = YES
utils/haddock_PACKAGE = haddock
utils/haddock_CONFIGURE_OPTS = --flag in-ghc-tree
utils/haddock_dist_SHELL_WRAPPER = YES
utils/haddock_dist_INSTALL = YES
utils/haddock_dist_INSTALL_INPLACE = YES
utils/haddock_dist_INSTALL_SHELL_WRAPPER_NAME = haddock-ghc-$(ProjectVersion)
utils/haddock_dist_PROGNAME = haddock

ifeq "$(GhcProfiled)" "YES"
utils/haddock_dist_PROGRAM_WAY = p
utils/haddock_dist_WAY = p
utils/haddock/dist/build/tmp/$(utils/haddock_dist_PROG) : $(utils/haddock_dist_p_LIB)
endif

ifeq "$(HADDOCK_DOCS)" "NO"
utils/haddock_dist_NOT_NEEDED = YES
endif

$(eval $(call build-prog,utils/haddock,dist,2))

ifneq "$(BINDIST)" "YES"

$(INPLACE_BIN)/$(utils/haddock_dist_PROG): $(INPLACE_LIB)/html $(INPLACE_LIB)/latex

$(INPLACE_LIB)/html:
	$(call removeTrees,$@)
	"$(CP)" -RL utils/haddock/haddock-api/resources/html $@

$(INPLACE_LIB)/latex:
	$(call removeTrees,$@)
	"$(CP)" -RL utils/haddock/haddock-api/resources/latex $@

endif

utils/haddock_dist_DATA_FILES += html/quick-jump.min.js
utils/haddock_dist_DATA_FILES += html/quick-jump.css
utils/haddock_dist_DATA_FILES += html/haddock-bundle.min.js
utils/haddock_dist_DATA_FILES += html/Classic.theme/haskell_icon.gif
utils/haddock_dist_DATA_FILES += html/Classic.theme/minus.gif
utils/haddock_dist_DATA_FILES += html/Classic.theme/plus.gif
utils/haddock_dist_DATA_FILES += html/Classic.theme/xhaddock.css
utils/haddock_dist_DATA_FILES += html/Ocean.theme/hslogo-16.png
utils/haddock_dist_DATA_FILES += html/Ocean.theme/minus.gif
utils/haddock_dist_DATA_FILES += html/Ocean.theme/ocean.css
utils/haddock_dist_DATA_FILES += html/Ocean.theme/plus.gif
utils/haddock_dist_DATA_FILES += html/Ocean.theme/synopsis.png
utils/haddock_dist_DATA_FILES += html/Linuwial.std-theme/linuwial.css
utils/haddock_dist_DATA_FILES += html/Linuwial.std-theme/synopsis.png
utils/haddock_dist_DATA_FILES += html/solarized.css
utils/haddock_dist_DATA_FILES += html/highlight.js
utils/haddock_dist_DATA_FILES += latex/haddock.sty

ifeq "$(HADDOCK_DOCS)" "YES"
install: install_utils/haddock_data
ifeq "$(Windows_Host)" "NO"
install: install_utils/haddock_link
endif
endif

.PHONY: install_utils/haddock_data
install_utils/haddock_data:
	$(foreach i,$(sort $(dir $(utils/haddock_dist_DATA_FILES))), \
	    $(call make-command,$(INSTALL_DIR) "$(DESTDIR)$(ghclibdir)/$i"))
	$(foreach i,$(utils/haddock_dist_DATA_FILES), \
	    $(call make-command,$(INSTALL_DATA) $(INSTALL_OPTS) utils/haddock/haddock-api/resources/$i "$(DESTDIR)$(ghclibdir)/$(dir $i)"))

.PHONY: install_utils/haddock_link
install_utils/haddock_link:
	$(call removeFiles,"$(DESTDIR)$(bindir)/haddock")
	$(LN_S) $(utils/haddock_dist_INSTALL_SHELL_WRAPPER_NAME) "$(DESTDIR)$(bindir)/haddock"

BINDIST_EXTRAS += $(addprefix utils/haddock/haddock-api/resources/,$(utils/haddock_dist_DATA_FILES))
