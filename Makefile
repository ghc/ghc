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

.PHONY: install
install:
	$(MAKE) -r --no-print-directory -f ghc.mk install BINDIST=YES NO_INCLUDE_DEPS=YES

.PHONY: show
show:
	$(MAKE) -r --no-print-directory -f ghc.mk $@

else

default : all
	@:

# No need to update makefiles for these targets:
REALGOALS=$(filter-out clean clean_% distclean maintainer-clean show,$(MAKECMDGOALS))

# NB. not the same as saying '%: ...', which doesn't do the right thing:
# it does nothing if we specify a target that already exists.
.PHONY: $(REALGOALS)
$(REALGOALS) all:
	@echo "===--- updating makefiles phase 0"
	$(MAKE) -r --no-print-directory -f ghc.mk phase=0 just-makefiles
	@echo "===--- updating makefiles phase 1"
	$(MAKE) -r --no-print-directory -f ghc.mk phase=1 just-makefiles
	@echo "===--- updating makefiles phase 2"
	$(MAKE) -r --no-print-directory -f ghc.mk phase=2 just-makefiles
	@echo "===--- updating makefiles phase 3"
	$(MAKE) -r --no-print-directory -f ghc.mk phase=3 just-makefiles
	@echo "===--- finished updating makefiles"
	$(MAKE) -r --no-print-directory -f ghc.mk $@

binary-dist:
	rm -f bindist-list
	$(MAKE) -r --no-print-directory -f ghc.mk bindist BINDIST=YES
	$(MAKE) -r --no-print-directory -f ghc.mk binary-dist

clean distclean maintainer-clean:
	$(MAKE) -r --no-print-directory -f ghc.mk $@
	test ! -d testsuite || $(MAKE) -C testsuite $@

$(filter clean_%, $(MAKECMDGOALS)) : clean_% :
	$(MAKE) -r --no-print-directory -f ghc.mk $@

show:
	$(MAKE) -r --no-print-directory -f ghc.mk $@

# If the user says 'make A B', then we don't want to invoke two
# instances of the rule above in parallel:
.NOTPARALLEL:

endif

