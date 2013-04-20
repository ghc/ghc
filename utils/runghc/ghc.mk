# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Modifying
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

ifneq "$(BINDIST)" "YES"
# hack: the build system has trouble with Main modules not called Main.hs
utils/runghc/dist-install/build/Main.hs : utils/runghc/runghc.hs | $$(dir $$@)/.
	"$(CP)" $< $@
endif

$(eval $(call build-prog,utils/runghc,dist-install,1))

install: install_runhaskell

.PHONY: install_runhaskell
ifeq "$(Windows_Host)" "YES"
install_runhaskell: install_bins
	"$(CP)" $(DESTDIR)$(bindir)/runghc$(exeext1) $(DESTDIR)$(bindir)/runhaskell$(exeext1)
else
install_runhaskell:
	$(call removeFiles,"$(DESTDIR)$(bindir)/runhaskell")
	$(LN_S) runghc "$(DESTDIR)$(bindir)/runhaskell"
	$(call removeFiles,"$(DESTDIR)$(bindir)/runghc")
	$(LN_S) runghc-$(ProjectVersion) "$(DESTDIR)$(bindir)/runghc"
endif

