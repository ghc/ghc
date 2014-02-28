# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
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
REALGOALS=$(filter-out binary-dist binary-dist-prep bootstrapping-files framework-pkg clean clean_% distclean maintainer-clean show echo help test fulltest,$(MAKECMDGOALS))

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
ifneq "$(OMIT_PHASE_0)" "YES"
	@echo "===--- building phase 0"
	$(MAKE) -r --no-print-directory -f ghc.mk phase=0 phase_0_builds
endif
ifneq "$(OMIT_PHASE_1)" "YES"
	@echo "===--- building phase 1"
	$(MAKE) -r --no-print-directory -f ghc.mk phase=1 phase_1_builds
endif
	@echo "===--- building final phase"
	$(MAKE) -r --no-print-directory -f ghc.mk phase=final $@

binary-dist: binary-dist-prep
	mv bindistprep/*.tar.$(TAR_COMP_EXT) .

binary-dist-prep:
ifeq "$(mingw32_TARGET_OS)" "1"
	$(MAKE) -r --no-print-directory -f ghc.mk windows-binary-dist-prep
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

bootstrapping-files show echo:
	$(MAKE) -r --no-print-directory -f ghc.mk $@

ifeq "$(darwin_TARGET_OS)" "1"
framework-pkg:
	$(MAKE) -C distrib/MacOS $@
endif

# If the user says 'make A B', then we don't want to invoke two
# instances of the rule above in parallel:
.NOTPARALLEL:

endif

.PHONY: test
test:
	$(MAKE) -C testsuite/tests CLEANUP=1 OUTPUT_SUMMARY=../../testsuite_summary.txt fast

.PHONY: fulltest
fulltest:
	$(MAKE) -C testsuite/tests CLEANUP=1 OUTPUT_SUMMARY=../../testsuite_summary.txt

