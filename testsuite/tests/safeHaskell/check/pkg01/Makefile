TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

cleanPackageDatabase.%:
	rm -rf pdb.$*

HERE := $(abspath .)
$(eval $(call canonicalise,HERE))

ifeq "$(GhcDynamicByDefault)" "YES"
HI_SUF = dyn_hi
else
HI_SUF = hi
endif

mkPackageDatabase.%:
	'$(MAKE)' cleanPackageDatabase.$*
	mkdir pdb.$*

	'$(TEST_HC)' -outputdir pdb.$* -o pdb.$*/setup Setup.hs -v0

	'$(GHC_PKG)' init pdb.$*/local.db
# We don't make use of -rtsopts in this test, and if it's enabled then
# we get a warning if dynlibs are enabled by default that:
#     Warning: -rtsopts and -with-rtsopts have no effect with -shared.
# so we filter the flag out
	pdb.$*/setup configure -v0 --dist pdb.$*/dist --prefix='$(HERE)/pdb.$*/install' --with-compiler='$(TEST_HC)' --ghc-options='$(filter-out -rtsopts,$(TEST_HC_OPTS)) -fpackage-trust -trust base -trust bytestring' --with-hc-pkg='$(GHC_PKG)' --package-db='pdb.$*/local.db' $(VANILLA) $(PROF) $(DYN)
	pdb.$*/setup build     -v0 --dist pdb.$*/dist
	pdb.$*/setup copy      -v0 --dist pdb.$*/dist
	pdb.$*/setup register  -v0 --dist pdb.$*/dist --inplace

# We use the global package database as there's no easy way to tell
# ghc-pkg (via Cabal) to use one in ., and the global one at least
# won't affect the installed GHC and is more likely to work

safePkg01_GHC_PKG = '$(GHC_PKG)' --no-user-package-db -f pdb.safePkg01/local.db

safePkg01:
	'$(MAKE)' mkPackageDatabase.safePkg01

	$(safePkg01_GHC_PKG) list
	$(safePkg01_GHC_PKG) field safePkg01-1.0 trusted
	echo
	echo 'M_SafePkg'
	'$(TEST_HC)' --show-iface pdb.safePkg01/dist/build/M_SafePkg.$(HI_SUF) | grep -E '^package dependencies:|^trusted:|^require own pkg trusted:'
	echo
	echo 'M_SafePkg2'
	'$(TEST_HC)' --show-iface pdb.safePkg01/dist/build/M_SafePkg2.$(HI_SUF) | grep -E '^package dependencies:|^trusted:|^require own pkg trusted:'
	echo
	echo 'M_SafePkg3'
	'$(TEST_HC)' --show-iface pdb.safePkg01/dist/build/M_SafePkg3.$(HI_SUF) | grep -E '^package dependencies:|^trusted:|^require own pkg trusted:'
	echo
	echo 'M_SafePkg4'
	'$(TEST_HC)' --show-iface pdb.safePkg01/dist/build/M_SafePkg4.$(HI_SUF) | grep -E '^package dependencies:|^trusted:|^require own pkg trusted:'
	echo
	echo 'M_SafePkg5'
	'$(TEST_HC)' --show-iface pdb.safePkg01/dist/build/M_SafePkg5.$(HI_SUF) | grep -E '^package dependencies:|^trusted:|^require own pkg trusted:'
	echo
	echo 'M_SafePkg6'
	'$(TEST_HC)' --show-iface pdb.safePkg01/dist/build/M_SafePkg6.$(HI_SUF) | grep -E '^package dependencies:|^trusted:|^require own pkg trusted:'
	echo
	echo 'M_SafePkg7'
	'$(TEST_HC)' --show-iface pdb.safePkg01/dist/build/M_SafePkg7.$(HI_SUF) | grep -E '^package dependencies:|^trusted:|^require own pkg trusted:'
	echo
	echo 'M_SafePkg8'
	'$(TEST_HC)' --show-iface pdb.safePkg01/dist/build/M_SafePkg8.$(HI_SUF) | grep -E '^package dependencies:|^trusted:|^require own pkg trusted:'
	echo
	echo 'Testing setting trust'
	$(safePkg01_GHC_PKG) trust    safePkg01-1.0
	$(safePkg01_GHC_PKG) field    safePkg01-1.0 trusted
	$(safePkg01_GHC_PKG) distrust safePkg01-1.0
	$(safePkg01_GHC_PKG) field    safePkg01-1.0 trusted
	$(safePkg01_GHC_PKG) distrust safePkg01-1.0
	$(safePkg01_GHC_PKG) field    safePkg01-1.0 trusted

