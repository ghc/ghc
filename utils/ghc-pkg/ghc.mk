# -----------------------------------------------------------------------------
#
# (c) 2009-2012 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/architecture
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/modifying
#
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Bootstrapping ghc-pkg

utils/ghc-pkg_PACKAGE = ghc-pkg

# Note [Why build certain utils twice?]
#
# We build certain utils twice: once with stage0, and once with stage1.
# Examples are ghc-pkg and hsc2hs.
#
# These tools are needed during the bootstrapping process, so we have to use
# stage0 to build them at first (stage1 doesn't exist yet). (side note: they're
# also used later in the build process). We install them inplace.
#
# But we can't install these copies when you run 'make install'. The reason is
# that when DYNAMIC_GHC_PROGRAMS=YES, we want to install copies that are
# dynamically linked. But the stage0 copies are either statically linked, or
# linked against libraries on the build machine.
#
# Therefore we build fresh copies, using the stage1 compiler, and install them
# when you run 'make install'. They are not used for any other purpose.

# -----------------------------------------------------------------------------
# Build ghc-pkg with the stage0 compiler in the dist directory, and install
# inplace. This is the copy we use during in-tree development.
utils/ghc-pkg_dist_USES_CABAL = YES
utils/ghc-pkg_dist_PROGNAME = ghc-pkg
utils/ghc-pkg_dist_SHELL_WRAPPER = YES
utils/ghc-pkg_dist_INSTALL_INPLACE = YES

# When cross-built ghc-stage2 is installed 'make install' needs to call
# native ghc-pkg (not the cross-built one) to register installed packages
# 'ghc-pkg_DIST_BINARY' variable only refer to native binary.
ghc-pkg_DIST_BINARY_NAME = ghc-pkg$(exeext0)
ghc-pkg_DIST_BINARY = utils/ghc-pkg/dist/build/tmp/$(ghc-pkg_DIST_BINARY_NAME)

# See Note [Stage1Only vs stage=1] in mk/config.mk.in.
ifeq "$(Stage1Only)" "YES"
# Install the copy of ghc-pkg from the dist directory when running 'make
# install' (it's the only copy we have at this stage).
utils/ghc-pkg_dist_INSTALL = YES
utils/ghc-pkg_dist_INSTALL_SHELL_WRAPPER_NAME = ghc-pkg-$(ProjectVersion)
endif

ifeq "$(WITH_TERMINFO)" "NO"
utils/ghc-pkg_dist-install_CONFIGURE_OPTS += -f-terminfo
endif

$(eval $(call build-prog,utils/ghc-pkg,dist,0))

# ghc-pkg uses `settings` to figure out the target platform to figure out a
# subdirectory for the user pkg db. So make sure `settings` exists (alterative
# is to specify global package db only.
$(ghc-pkg_INPLACE) : | $(INPLACE_PACKAGE_CONF)/. $(INPLACE_LIB)/settings

# -----------------------------------------------------------------------------
# Build another copy of ghc-pkg with the stage1 compiler in the dist-install
# directory. Don't install it inplace (we use the dist copy there), but do
# install it when running 'make install'.
#
# See Note [Why build certain utils twice?].

# See Note [Stage1Only vs stage=1] in mk/config.mk.in.
ifneq "$(Stage1Only)" "YES"
utils/ghc-pkg_dist-install_USES_CABAL = YES
utils/ghc-pkg_dist-install_PROGNAME = ghc-pkg
utils/ghc-pkg_dist-install_SHELL_WRAPPER = YES
utils/ghc-pkg_dist-install_INSTALL_INPLACE = NO
utils/ghc-pkg_dist-install_INSTALL = YES
utils/ghc-pkg_dist-install_INSTALL_SHELL_WRAPPER_NAME = ghc-pkg-$(ProjectVersion)

$(eval $(call build-prog,utils/ghc-pkg,dist-install,1))
endif

# -----------------------------------------------------------------------------
# Link ghc-pkg to ghc-pkg-$(ProjectVersion) when installing

ifeq "$(Windows_Host)" "NO"
install: install_utils/ghc-pkg_link

.PHONY: install_utils/ghc-pkg_link
install_utils/ghc-pkg_link: 
	$(INSTALL_DIR) "$(DESTDIR)$(bindir)"
	$(call removeFiles,"$(DESTDIR)$(bindir)/$(CrossCompilePrefix)ghc-pkg")
	$(LN_S) $(CrossCompilePrefix)ghc-pkg-$(ProjectVersion) "$(DESTDIR)$(bindir)/$(CrossCompilePrefix)ghc-pkg"
endif
