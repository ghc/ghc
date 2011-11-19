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
	echo '$(TOP)/$< --global-conf $$PKGCONF $${1+"$$@"}' >> $@
	chmod +x $@
endif

else

$(GHC_PKG_INPLACE) : utils/ghc-pkg/dist/build/$(utils/ghc-pkg_dist_PROG)$(exeext) | $$(dir $$@)/. $(INPLACE_PACKAGE_CONF)/.
	$(call removeFiles,$(wildcard $(INPLACE_PACKAGE_CONF)/*))
ifeq "$(Windows)" "YES"
	cp $< $@
else
	$(call removeFiles,$@)
	echo "#!/bin/sh" >>$@
	echo "PKGCONF=$(TOP)/$(INPLACE_PACKAGE_CONF)" >>$@
	echo '$(TOP)/$< --global-conf $$PKGCONF $${1+"$$@"}' >> $@
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
utils/ghc-pkg/dist/build/$(utils/ghc-pkg_dist_PROG)$(exeext): utils/ghc-pkg/Main.hs utils/ghc-pkg/Version.hs | bootstrapping/. $$(dir $$@)/. $(GHC_CABAL_INPLACE) 
	"$(GHC)" $(SRC_HC_OPTS) --make utils/ghc-pkg/Main.hs -o $@ \
	       -no-user-package-conf \
	       -Wall -fno-warn-unused-imports -fno-warn-warnings-deprecations \
	       -DCABAL_VERSION=$(CABAL_VERSION) \
	       -DBOOTSTRAPPING \
	       -odir  bootstrapping \
	       -hidir bootstrapping \
               -iutils/ghc-pkg \
	       -XCPP -XExistentialQuantification -XDeriveDataTypeable \
	       -ilibraries/Cabal/Cabal \
	       -ilibraries/filepath \
	       -ilibraries/extensible-exceptions \
	       -ilibraries/hpc \
	       -ilibraries/binary/src \
	       -ilibraries/bin-package-db


utils/ghc-pkg/Version.hs: mk/project.mk
	$(call removeFiles,$@)
	echo "module Version where"                    >> $@
	echo "version, targetOS, targetARCH :: String" >> $@
	echo "version    = \"$(ProjectVersion)\""      >> $@
	echo "targetOS   = \"$(TargetOS_CPP)\""        >> $@
	echo "targetARCH = \"$(TargetArch_CPP)\""      >> $@

$(eval $(call clean-target,utils/ghc-pkg,dist,\
   utils/ghc-pkg/dist \
   utils/ghc-pkg/Version.hs))

# -----------------------------------------------------------------------------
# Building ghc-pkg with stage 1

utils/ghc-pkg_dist-install_USES_CABAL = YES
utils/ghc-pkg_PACKAGE = ghc-pkg

utils/ghc-pkg_dist-install_PROG = ghc-pkg
utils/ghc-pkg_dist-install_SHELL_WRAPPER = YES
utils/ghc-pkg_dist-install_INSTALL_SHELL_WRAPPER = YES
utils/ghc-pkg_dist-install_INSTALL_SHELL_WRAPPER_NAME = ghc-pkg-$(ProjectVersion)
utils/ghc-pkg_dist-install_INSTALL_INPLACE = NO

ifeq "$(BootingFromHc)" "YES"
utils/ghc-pkg_dist-install_OTHER_OBJS += $(ALL_STAGE1_LIBS) $(ALL_STAGE1_LIBS) $(ALL_STAGE1_LIBS) $(ALL_RTS_LIBS) $(libffi_STATIC_LIB)
endif

$(eval $(call build-prog,utils/ghc-pkg,dist-install,1))

ifeq "$(Windows)" "NO"
install: install_utils/ghc-pkg_link

.PNONY: install_utils/ghc-pkg_link
install_utils/ghc-pkg_link: 
	$(call INSTALL_DIR,"$(DESTDIR)$(bindir)")
	$(call removeFiles,"$(DESTDIR)$(bindir)/ghc-pkg")
	$(LN_S) ghc-pkg-$(ProjectVersion) "$(DESTDIR)$(bindir)/ghc-pkg"
endif

