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

# Eliminate use of the built-in implicit rules, and clear out the default list
# of suffixes for suffix rules. Speeds up make quite a bit. Both are needed
# for the shortest `make -d` output.
# Don't set --no-builtin-variables; some rules might stop working if you do
# (e.g. 'make clean' in testsuite/ currently relies on an implicit $RM).
MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

ifeq "$(wildcard distrib/)" ""

# We're in a bindist

.PHONY: default
default:
	@echo 'Run "make install" to install'
	@false

.PHONY: install show
install show:
	$(MAKE) --no-print-directory -f ghc.mk $@ BINDIST=YES NO_INCLUDE_DEPS=YES

else

.PHONY: default
default : all
	@:

# For help, type 'make help'
.PHONY: help
help:
	@cat MAKEHELP.md

ifneq "$(filter maintainer-clean distclean clean clean_% help,$(MAKECMDGOALS))" ""
-include mk/config.mk
else
include mk/config.mk
ifeq "$(ProjectVersion)" ""
$(error Please run ./configure first)
endif
endif

include mk/custom-settings.mk

# Verify that stage 0 LLVM backend isn't affected by Bug #9439 if needed
ifeq "$(GHC_LLVM_AFFECTED_BY_9439)" "1"
ifneq "$(findstring -fllvm,$(GhcHcOpts) $(GhcStage1HcOpts))" ""
$(error Stage 0 compiler is affected by Bug #9439. Refusing to bootstrap with -fllvm)
endif
endif

# No need to update makefiles for these targets:
# (the ones we're filtering out)
REALGOALS=$(filter-out \
    binary-dist \
    binary-dist-prep \
    sdist sdist-ghc \
    sdist-ghc-prep \
    sdist-windows-tarballs \
    sdist-windows-tarballs-prep \
    sdist-testsuite \
    sdist-testsuite-prep \
    bootstrapping-files \
    framework-pkg \
    clean \
    clean_% \
    distclean \
    maintainer-clean \
    show \
    show! \
    echo \
    help \
    test \
    fulltest \
    fasttest \
  ,$(MAKECMDGOALS))

# configure touches certain files even if they haven't changed.  This
# can mean a lot of unnecessary recompilation after a re-configure, so
# here we cache the old versions of these files so we can restore the
# timestamps.
%.old:  %
	@set -x && test -f $@ && cmp -s $< $@ || cp -p $< $@
	touch -r $@ $<


# NB. not the same as saying '%: ...', which doesn't do the right thing:
# it does nothing if we specify a target that already exists.
.PHONY: $(REALGOALS) all
$(REALGOALS) all: mk/config.mk.old mk/project.mk.old compiler/ghc.cabal.old
ifneq "$(OMIT_PHASE_0)" "YES"
	@echo "===--- building phase 0"
	$(MAKE) --no-print-directory -f ghc.mk phase=0 phase_0_builds
endif
ifneq "$(OMIT_PHASE_1)" "YES"
	@echo "===--- building phase 1"
	$(MAKE) --no-print-directory -f ghc.mk phase=1 phase_1_builds
endif
	@echo "===--- building final phase"
	$(MAKE) --no-print-directory -f ghc.mk phase=final $@

.PHONY: binary-dist
binary-dist: binary-dist-prep
	mv bindistprep/*.tar.$(TAR_COMP_EXT) .

.PHONY: binary-dist-prep
binary-dist-prep:
ifeq "$(mingw32_TARGET_OS)" "1"
	$(MAKE) --no-print-directory -f ghc.mk windows-binary-dist-prep
else
	rm -f bindist-list
	$(MAKE) --no-print-directory -f ghc.mk bindist-list BINDIST=YES
	$(MAKE) --no-print-directory -f ghc.mk unix-binary-dist-prep
endif

.PHONY: sdist sdist-ghc sdist-ghc-prep sdist-windows-tarballs sdist-windows-tarballs-prep sdist-testsuite sdist-testsuite-prep
# Just running `./boot && ./configure && make sdist` should work, so skip
# phase 0 and 1 and don't build any dependency files.
sdist sdist-ghc sdist-ghc-prep sdist-windows-tarballs sdist-windows-tarballs-prep sdist-testsuite sdist-testsuite-prep :
	$(MAKE) --no-print-directory -f ghc.mk $@ NO_INCLUDE_DEPS=YES NO_INCLUDE_PKGDATA=YES

.PHONY: clean distclean maintainer-clean
clean distclean maintainer-clean:
	$(MAKE) --no-print-directory -f ghc.mk $@ CLEANING=YES
	test ! -d testsuite || $(MAKE) -C testsuite $@

.PHONY: $(filter clean_%,$(MAKECMDGOALS))
$(filter clean_%, $(MAKECMDGOALS)) : clean_% :
	$(MAKE) --no-print-directory -f ghc.mk $@ CLEANING=YES

.PHONY: bootstrapping-files show echo
bootstrapping-files show echo:
	$(MAKE) --no-print-directory -f ghc.mk $@

.PHONY: show!
show!:
	$(MAKE) --no-print-directory -f ghc.mk show NO_INCLUDE_PKGDATA=YES

ifeq "$(darwin_TARGET_OS)" "1"
.PHONY: framework-pkg
framework-pkg:
	$(MAKE) -C distrib/MacOS $@
endif

# If the user says 'make A B', then we don't want to invoke two
# instances of the rule above in parallel:
.NOTPARALLEL:

endif

.PHONY: fasttest
fasttest:
	$(MAKE) -C testsuite/tests CLEANUP=1 SUMMARY_FILE=../../testsuite_summary.txt fast

.PHONY: fulltest test
fulltest test:
	$(MAKE) -C testsuite/tests CLEANUP=1 SUMMARY_FILE=../../testsuite_summary.txt
