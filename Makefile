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
 
ifeq "$(wildcard distrib/)" ""

# We're in a bindist

.PHONY: default
default:
	@echo 'Run "make install" to install'
	@false

.PHONY: install show
install show:
	$(MAKE) -r --no-print-directory -f ghc.mk $@ BINDIST=YES NO_INCLUDE_DEPS=YES

else

default : all
	@:

# For help, type 'make help'
.PHONY: help
help :
	@cat MAKEHELP

ifneq "$(findstring clean,$(MAKECMDGOALS))" ""
-include mk/config.mk
else
include mk/config.mk
ifeq "$(ProjectVersion)" ""
$(error Please run ./configure first)
endif
endif

include mk/custom-settings.mk

# No need to update makefiles for these targets:
REALGOALS=$(filter-out binary-dist binary-dist-prep bootstrapping-files framework-pkg clean clean_% distclean maintainer-clean show help install-docs,$(MAKECMDGOALS))

# configure touches certain files even if they haven't changed.  This
# can mean a lot of unnecessary recompilation after a re-configure, so
# here we cache the old versions of these files so we can restore the
# timestamps.
%.old:  %
	@set -x && test -f $@ && cmp -s $< $@ || cp -p $< $@
	touch -r $@ $<


# NB. not the same as saying '%: ...', which doesn't do the right thing:
# it does nothing if we specify a target that already exists.
.PHONY: $(REALGOALS)
$(REALGOALS) all: mk/config.mk.old mk/project.mk.old compiler/ghc.cabal.old
	@echo "===--- updating makefiles phase 0"
	$(MAKE) -r --no-print-directory -f ghc.mk phase=0 just-makefiles
ifneq "$(OMIT_PHASE_1)" "YES"
	@echo "===--- updating makefiles phase 1"
	$(MAKE) -r --no-print-directory -f ghc.mk phase=1 just-makefiles
endif
ifneq "$(OMIT_PHASE_2)" "YES"
	@echo "===--- updating makefiles phase 2"
	$(MAKE) -r --no-print-directory -f ghc.mk phase=2 just-makefiles
endif
ifneq "$(OMIT_PHASE_3)" "YES"
	@echo "===--- updating makefiles phase 3"
	$(MAKE) -r --no-print-directory -f ghc.mk phase=3 just-makefiles
endif
	@echo "===--- finished updating makefiles"
	$(MAKE) -r --no-print-directory -f ghc.mk $@

binary-dist: binary-dist-prep
ifeq "$(mingw32_TARGET_OS)" "1"
	mv bindistprep/*.exe .
endif
	mv bindistprep/*.tar.bz2 .

binary-dist-prep:
ifeq "$(mingw32_TARGET_OS)" "1"
	$(MAKE) -r --no-print-directory -f ghc.mk windows-binary-dist-prep
	$(MAKE) -r --no-print-directory -f ghc.mk windows-installer
else
	rm -f bindist-list
	$(MAKE) -r --no-print-directory -f ghc.mk bindist BINDIST=YES
	$(MAKE) -r --no-print-directory -f ghc.mk unix-binary-dist-prep
endif

clean distclean maintainer-clean:
	$(MAKE) -r --no-print-directory -f ghc.mk $@ CLEANING=YES
	test ! -d testsuite || $(MAKE) -C testsuite $@

$(filter clean_%, $(MAKECMDGOALS)) : clean_% :
	$(MAKE) -r --no-print-directory -f ghc.mk $@ CLEANING=YES

bootstrapping-files show:
	$(MAKE) -r --no-print-directory -f ghc.mk $@

ifeq "$(darwin_TARGET_OS)" "1"
framework-pkg:
	$(MAKE) -C distrib/MacOS $@
endif

# install-docs is a historical target that isn't supported in GHC 6.12. See #3662.
install-docs:
	@echo "The install-docs target is not supported in GHC 6.12.1 and later."
	@echo "'make install' now installs everything, including documentation."
	@exit 1

# If the user says 'make A B', then we don't want to invoke two
# instances of the rule above in parallel:
.NOTPARALLEL:

endif
