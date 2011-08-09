TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -rf setup a.out
	rm -rf dist
	find . -name "*.o" | xargs rm -f
	find . -name "*.hi" | xargs rm -f
	rm -rf local.db
	rm -rf install

# We use the global package database as there's no easy way to tell
# ghc-pkg (via Cabal) to use one in ., and the global one at least
# won't affect the installed GHC and is more likely to work

PREFIX := $(abspath install)
$(eval $(call canonicalise,PREFIX))

PKGCONF = local.db
LGHC_PKG = '$(GHC_PKG)' --no-user-package-conf -f '$(PKGCONF)'

safePkg01:
	'$(MAKE)' clean
	'$(TEST_HC)' --make -o setup Setup.hs -v0

	'$(GHC_PKG)' init local.db
	./setup configure -v0 --prefix='$(PREFIX)' --with-compiler='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS) -trust base' --with-hc-pkg='$(GHC_PKG)' --package-db='$(PKGCONF)' $(PROF)
	./setup build -v0
	./setup copy -v0
	./setup register --inplace -v0
	$(LGHC_PKG) list
	$(LGHC_PKG) field safePkg01-1.0 trusted
	echo
	echo 'M_SafePkg'
	'$(TEST_HC)' --show-iface dist/build/M_SafePkg.hi | grep -E '^package dependencies:|^trusted:|^require own pkg trusted:'
	echo
	echo 'M_SafePkg2'
	'$(TEST_HC)' --show-iface dist/build/M_SafePkg2.hi | grep -E '^package dependencies:|^trusted:|^require own pkg trusted:'
	echo
	echo 'M_SafePkg3'
	'$(TEST_HC)' --show-iface dist/build/M_SafePkg3.hi | grep -E '^package dependencies:|^trusted:|^require own pkg trusted:'
	echo
	echo 'M_SafePkg4'
	'$(TEST_HC)' --show-iface dist/build/M_SafePkg4.hi | grep -E '^package dependencies:|^trusted:|^require own pkg trusted:'
	echo
	echo 'Testing setting trust'
	$(LGHC_PKG) distrust safePkg01-1.0
	$(LGHC_PKG) field safePkg01-1.0 trusted
