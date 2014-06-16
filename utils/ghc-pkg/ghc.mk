# -----------------------------------------------------------------------------
#
# (c) 2009-2012 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Bootstrapping ghc-pkg

utils/ghc-pkg/dist/build/Version.hs \
utils/ghc-pkg/dist-install/build/Version.hs: mk/project.mk | $$(dir $$@)/.
	$(call removeFiles,$@)
	echo "module Version where"                    >> $@
	echo "version, targetOS, targetARCH :: String" >> $@
	echo "version    = \"$(ProjectVersion)\""      >> $@
	echo "targetOS   = \"$(TargetOS_CPP)\""        >> $@
	echo "targetARCH = \"$(TargetArch_CPP)\""      >> $@

utils/ghc-pkg_PACKAGE = ghc-pkg

# -----------------------------------------------------------------------------
# Cross-compile case: install our dist version

ifeq "$(Stage1Only)" "YES"

utils/ghc-pkg_dist_INSTALL = YES
utils/ghc-pkg_dist_SHELL_WRAPPER = YES
utils/ghc-pkg_dist_INSTALL_SHELL_WRAPPER_NAME = ghc-pkg-$(ProjectVersion)
utils/ghc-pkg_dist_WANT_INSTALLED_WRAPPER = YES

$(eval $(call shell-wrapper,utils/ghc-pkg,dist))

endif

utils/ghc-pkg_dist_USES_CABAL = YES
utils/ghc-pkg_dist_PROGNAME = ghc-pkg
utils/ghc-pkg_dist_SHELL_WRAPPER = YES
utils/ghc-pkg_dist_INSTALL_INPLACE = YES

$(eval $(call build-prog,utils/ghc-pkg,dist,0))

$(ghc-pkg_INPLACE) : | $(INPLACE_PACKAGE_CONF)/.

utils/ghc-pkg/dist/package-data.mk: \
    utils/ghc-pkg/dist/build/Version.hs

# -----------------------------------------------------------------------------
# Normal case: Build ghc-pkg with stage 1 and install it

ifneq "$(Stage1Only)" "YES"

utils/ghc-pkg_dist-install_USES_CABAL = YES

utils/ghc-pkg_dist-install_PROGNAME = ghc-pkg
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

ifeq "$(Windows_Host)" "NO"
install: install_utils/ghc-pkg_link

.PHONY: install_utils/ghc-pkg_link
install_utils/ghc-pkg_link: 
	$(call INSTALL_DIR,"$(DESTDIR)$(bindir)")
	$(call removeFiles,"$(DESTDIR)$(bindir)/$(CrossCompilePrefix)ghc-pkg")
	$(LN_S) $(CrossCompilePrefix)ghc-pkg-$(ProjectVersion) "$(DESTDIR)$(bindir)/$(CrossCompilePrefix)ghc-pkg"
endif
