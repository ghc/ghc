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
# Cross-compile case: Install our dist version
# Normal case: Build ghc-pkg with stage 1

ifeq "$(Stage1Only)" "YES"
GHC_PKG_DISTDIR=dist
else
GHC_PKG_DISTDIR=dist-install
endif

utils/ghc-pkg_$(GHC_PKG_DISTDIR)_USES_CABAL = YES
utils/ghc-pkg_PACKAGE = ghc-pkg

utils/ghc-pkg_$(GHC_PKG_DISTDIR)_PROG = ghc-pkg
utils/ghc-pkg_$(GHC_PKG_DISTDIR)_SHELL_WRAPPER = YES
utils/ghc-pkg_$(GHC_PKG_DISTDIR)_INSTALL = YES
utils/ghc-pkg_$(GHC_PKG_DISTDIR)_INSTALL_SHELL_WRAPPER_NAME = ghc-pkg-$(ProjectVersion)
utils/ghc-pkg_$(GHC_PKG_DISTDIR)_INSTALL_INPLACE = NO

ifeq "$(BootingFromHc)" "YES"
utils/ghc-pkg_dist-install_OTHER_OBJS += $(ALL_STAGE1_LIBS) $(ALL_STAGE1_LIBS) $(ALL_STAGE1_LIBS) $(ALL_RTS_LIBS) $(libffi_STATIC_LIB)
endif

ifeq "$(Stage1Only)" "YES"
$(eval $(call shell-wrapper,utils/ghc-pkg,dist))
else
$(eval $(call build-prog,utils/ghc-pkg,dist-install,1))
endif

utils/ghc-pkg/dist-install/package-data.mk: \
    utils/ghc-pkg/dist-install/build/Version.hs

ifeq "$(Windows)" "NO"
install: install_utils/ghc-pkg_link

.PHONY: install_utils/ghc-pkg_link
install_utils/ghc-pkg_link: 
	$(call INSTALL_DIR,"$(DESTDIR)$(bindir)")
	$(call removeFiles,"$(DESTDIR)$(bindir)/$(CrossCompilePrefix)ghc-pkg")
	$(LN_S) $(CrossCompilePrefix)ghc-pkg-$(ProjectVersion) "$(DESTDIR)$(bindir)/$(CrossCompilePrefix)ghc-pkg"
endif

