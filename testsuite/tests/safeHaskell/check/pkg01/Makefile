TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

cleanSafePkg01:
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
LGHC_PKG = '$(GHC_PKG)' --no-user-package-db -f '$(PKGCONF)'

ifeq "$(GhcDynamicByDefault)" "YES"
HI_SUF = dyn_hi
else
HI_SUF = hi
endif

safePkg01:
	'$(MAKE)' cleanSafePkg01
	'$(TEST_HC)' --make -o setup Setup.hs -v0

	'$(GHC_PKG)' init local.db
# We don't make use of -rtsopts in this test, and if it's enabled then
# we get a warning if dynlibs are enabled by default that:
# 	Warning: -rtsopts and -with-rtsopts have no effect with -shared.
# so we filter the flag out
	./setup configure -v0 --prefix='$(PREFIX)' --with-compiler='$(TEST_HC)' --ghc-options='$(filter-out -rtsopts,$(TEST_HC_OPTS)) -fpackage-trust -trust base -trust bytestring' --with-hc-pkg='$(GHC_PKG)' --package-db='$(PKGCONF)' $(VANILLA) $(PROF) $(DYN)
	./setup build -v0
	./setup copy -v0
	./setup register --inplace -v0
	$(LGHC_PKG) list
	$(LGHC_PKG) field safePkg01-1.0 trusted
	echo
	echo 'M_SafePkg'
	'$(TEST_HC)' --show-iface dist/build/M_SafePkg.$(HI_SUF) | grep -E '^package dependencies:|^trusted:|^require own pkg trusted:'
	echo
	echo 'M_SafePkg2'
	'$(TEST_HC)' --show-iface dist/build/M_SafePkg2.$(HI_SUF) | grep -E '^package dependencies:|^trusted:|^require own pkg trusted:'
	echo
	echo 'M_SafePkg3'
	'$(TEST_HC)' --show-iface dist/build/M_SafePkg3.$(HI_SUF) | grep -E '^package dependencies:|^trusted:|^require own pkg trusted:'
	echo
	echo 'M_SafePkg4'
	'$(TEST_HC)' --show-iface dist/build/M_SafePkg4.$(HI_SUF) | grep -E '^package dependencies:|^trusted:|^require own pkg trusted:'
	echo
	echo 'M_SafePkg5'
	'$(TEST_HC)' --show-iface dist/build/M_SafePkg5.$(HI_SUF) | grep -E '^package dependencies:|^trusted:|^require own pkg trusted:'
	echo
	echo 'M_SafePkg6'
	'$(TEST_HC)' --show-iface dist/build/M_SafePkg6.$(HI_SUF) | grep -E '^package dependencies:|^trusted:|^require own pkg trusted:'
	echo
	echo 'M_SafePkg7'
	'$(TEST_HC)' --show-iface dist/build/M_SafePkg7.$(HI_SUF) | grep -E '^package dependencies:|^trusted:|^require own pkg trusted:'
	echo
	echo 'M_SafePkg8'
	'$(TEST_HC)' --show-iface dist/build/M_SafePkg8.$(HI_SUF) | grep -E '^package dependencies:|^trusted:|^require own pkg trusted:'
	echo
	echo 'Testing setting trust'
	$(LGHC_PKG) trust safePkg01-1.0
	$(LGHC_PKG) field safePkg01-1.0 trusted
	$(LGHC_PKG) distrust safePkg01-1.0
	$(LGHC_PKG) field safePkg01-1.0 trusted
	$(LGHC_PKG) distrust safePkg01-1.0
	$(LGHC_PKG) field safePkg01-1.0 trusted

