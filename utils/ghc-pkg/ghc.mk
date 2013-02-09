# -----------------------------------------------------------------------------
#
# (c) 2009-2012 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Bootstrapping ghc-pkg

utils/ghc-pkg_dist_PROG = ghc-pkg$(exeext)

ifeq "$(BootingFromHc)" "YES"

inplace/bin/ghc-pkg : utils/ghc-pkg/dist-install/build/tmp/$(utils/ghc-pkg_dist_PROG)$(exeext)
ifeq "$(Windows)" "YES"
	cp $< $@
else
	$(call removeFiles,$@)
	echo "#!/bin/sh" >>$@
	echo "PKGCONF=$(TOP)/$(INPLACE_PACKAGE_CONF)" >>$@
	echo '$(TOP)/$< --global-package-db $$PKGCONF $${1+"$$@"}' >> $@
	chmod +x $@
endif

else

$(GHC_PKG_INPLACE) : utils/ghc-pkg/dist/build/tmp/$(utils/ghc-pkg_dist_PROG)$(exeext) | $$(dir $$@)/. $(INPLACE_PACKAGE_CONF)/.
	$(call removeFiles,$(wildcard $(INPLACE_PACKAGE_CONF)/*))
ifeq "$(Windows)" "YES"
	cp $< $@
else
	$(call removeFiles,$@)
	echo "#!/bin/sh" >>$@
	echo "PKGCONF=$(TOP)/$(INPLACE_PACKAGE_CONF)" >>$@
	echo '$(TOP)/$< --global-package-db $$PKGCONF $${1+"$$@"}' >> $@
	chmod +x $@
endif

endif

# depend on ghc-cabal, otherwise we build Cabal twice when building in parallel.
# (ghc-cabal is an order-only dependency, we don't need to rebuild ghc-pkg
# if ghc-cabal is newer).
# The binary package is not warning-clean, so we need a few -fno-warns here.
#
# ToDo: we might want to do this using ghc-cabal instead.
#
utils/ghc-pkg/dist/build/tmp/$(utils/ghc-pkg_dist_PROG)$(exeext): utils/ghc-pkg/Main.hs utils/ghc-pkg/dist/build/Version.hs | bootstrapping/. $$(dir $$@)/. $(GHC_CABAL_INPLACE) 
	"$(GHC)" $(SRC_HC_OPTS) --make utils/ghc-pkg/Main.hs -o $@ \
	       -no-user-$(GHC_PACKAGE_DB_FLAG) \
	       -Wall -fno-warn-unused-imports -fno-warn-warnings-deprecations \
	       $(SRC_HC_WARNING_OPTS) \
	       -DCABAL_VERSION=$(CABAL_VERSION) \
	       -DBOOTSTRAPPING \
	       -odir  bootstrapping \
	       -hidir bootstrapping \
	       -iutils/ghc-pkg \
	       -iutils/ghc-pkg/dist/build \
	       -ilibraries/Cabal/Cabal \
	       -ilibraries/filepath \
	       -ilibraries/hpc \
	       -ilibraries/binary/src \
	       -ilibraries/bin-package-db


utils/ghc-pkg/dist/build/Version.hs \
utils/ghc-pkg/dist-install/build/Version.hs: mk/project.mk | $$(dir $$@)/.
	$(call removeFiles,$@)
	echo "module Version where"                    >> $@
	echo "version, targetOS, targetARCH :: String" >> $@
	echo "version    = \"$(ProjectVersion)\""      >> $@
	echo "targetOS   = \"$(TargetOS_CPP)\""        >> $@
	echo "targetARCH = \"$(TargetArch_CPP)\""      >> $@

$(eval $(call clean-target,utils/ghc-pkg,dist,utils/ghc-pkg/dist))

# -----------------------------------------------------------------------------
# Cross-compile case: install our dist version

ifeq "$(Stage1Only)" "YES"

utils/ghc-pkg_dist_INSTALL = YES
utils/ghc-pkg_dist_SHELL_WRAPPER = YES
utils/ghc-pkg_dist_INSTALL_SHELL_WRAPPER_NAME = ghc-pkg-$(ProjectVersion)
utils/ghc-pkg_dist_WANT_INSTALLED_WRAPPER = YES

INSTALL_LIBEXECS += utils/ghc-pkg/dist/build/tmp/$(utils/ghc-pkg_dist_PROG)

$(eval $(call shell-wrapper,utils/ghc-pkg,dist))

endif

# -----------------------------------------------------------------------------
# Normal case: Build ghc-pkg with stage 1 and install it

ifneq "$(Stage1Only)" "YES"

utils/ghc-pkg_dist-install_USES_CABAL = YES
utils/ghc-pkg_PACKAGE = ghc-pkg

utils/ghc-pkg_dist-install_PROG = ghc-pkg
utils/ghc-pkg_dist-install_SHELL_WRAPPER = YES
utils/ghc-pkg_dist-install_INSTALL = YES
utils/ghc-pkg_dist-install_INSTALL_SHELL_WRAPPER_NAME = ghc-pkg-$(ProjectVersion)
utils/ghc-pkg_dist-install_INSTALL_INPLACE = NO

$(eval $(call build-prog,utils/ghc-pkg,dist-install,1))

utils/ghc-pkg/dist-install/package-data.mk: \
    utils/ghc-pkg/dist-install/build/Version.hs

endif

# -----------------------------------------------------------------------------
# Link ghc-pkg to ghc-pkg-$(ProjectVersion) when installing

ifeq "$(Windows)" "NO"
install: install_utils/ghc-pkg_link

.PHONY: install_utils/ghc-pkg_link
install_utils/ghc-pkg_link: 
	$(call INSTALL_DIR,"$(DESTDIR)$(bindir)")
	$(call removeFiles,"$(DESTDIR)$(bindir)/$(CrossCompilePrefix)ghc-pkg")
	$(LN_S) $(CrossCompilePrefix)ghc-pkg-$(ProjectVersion) "$(DESTDIR)$(bindir)/$(CrossCompilePrefix)ghc-pkg"
endif
