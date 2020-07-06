TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -rf setup a.out dist/build/testA
	rm -rf ,tmp dist ,tmp2
	find . -name "*.o" |xargs rm -f
	find . -name "*.hi" |xargs rm -f
	rm -fr install-tmp
	rm -fr install
	rm -f .setup-config .installed-pkg-config
	rm -rf local.db

# We use the global package database as there's no easy way to tell
# ghc-pkg (via Cabal) to use one in ., and the global one at least
# won't affect the installed GHC and is more likely to work

PREFIX := $(abspath install)
$(eval $(call canonicalise,PREFIX))

SETUP='$(TEST_WRAPPER)' '$(PWD)/Setup' -v0

cabal01:
	$(MAKE) -s --no-print-directory clean
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -o Setup Setup.lhs -v0

	'$(GHC_PKG)' init local.db
# We don't make use of -rtsopts in this test, and if it's enabled then
# we get a warning if dynlibs are enabled by default that:
# 	Warning: -rtsopts and -with-rtsopts have no effect with -shared.
# so we filter the flag out
	$(SETUP) configure --prefix="$(PREFIX)" --with-compiler='$(TEST_HC)' --with-strip='$(STRIP)' --ghc-options='$(filter-out -rtsopts,$(TEST_HC_OPTS))' --with-hc-pkg='$(GHC_PKG)' --package-db=local.db $(VANILLA) $(PROF) $(DYN) --libsubdir='$$pkgid'
	$(SETUP) build
	$(SETUP) copy
	echo install1:
	ls -1 install
	rm -r install
#	install w/ register!
	$(SETUP) install
	echo install2:
	ls -1 install
	$(SETUP) sdist
	echo dist:
	ls -1 dist
	if [ "$(CLEANUP)" != "" ]; then $(MAKE) -s --no-print-directory clean; fi
