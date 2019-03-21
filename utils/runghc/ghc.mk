# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/architecture
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/modifying
#
# -----------------------------------------------------------------------------

utils/runghc_PACKAGE = runghc
utils/runghc_dist-install_USES_CABAL = YES
utils/runghc_dist-install_PROGNAME = runghc
utils/runghc_dist-install_SHELL_WRAPPER = YES
utils/runghc_dist-install_INSTALL = YES
utils/runghc_dist-install_INSTALL_INPLACE = YES
utils/runghc_dist-install_INSTALL_SHELL_WRAPPER_NAME = runghc-$(ProjectVersion)
utils/runghc_dist-install_EXTRA_HC_OPTS = -cpp -DVERSION="\"$(ProjectVersion)\""

# Be explicit about which version of ghc to call (#9054).
define utils/runghc_dist-install_INPLACE_SHELL_WRAPPER_EXTRA
echo 'ghcprog="$(ghc_stage2_INPLACE_SHELL_WRAPPER_NAME)"' >> "$(WRAPPER)"
endef
define utils/runghc_dist-install_INSTALL_SHELL_WRAPPER_EXTRA
echo 'ghcprog="$(ghc_stage$(INSTALL_GHC_STAGE)_INSTALL_SHELL_WRAPPER_NAME)"' >> "$(WRAPPER)"
endef

$(eval $(call build-prog,utils/runghc,dist-install,1))

install: install_runhaskell

.PHONY: install_runhaskell
ifeq "$(Windows_Host)" "YES"
install_runhaskell: install_bins
	"$(CP)" $(DESTDIR)$(bindir)/$(CrossCompilePrefix)runghc$(exeext1) $(DESTDIR)$(bindir)/$(CrossCompilePrefix)runhaskell$(exeext1)
else
install_runhaskell:
	$(call removeFiles,"$(DESTDIR)$(bindir)/$(CrossCompilePrefix)runhaskell")
	$(LN_S) $(CrossCompilePrefix)runghc "$(DESTDIR)$(bindir)/$(CrossCompilePrefix)runhaskell"
	$(call removeFiles,"$(DESTDIR)$(bindir)/$(CrossCompilePrefix)runghc")
	$(LN_S) $(CrossCompilePrefix)runghc-$(ProjectVersion) "$(DESTDIR)$(bindir)/$(CrossCompilePrefix)runghc"
endif
