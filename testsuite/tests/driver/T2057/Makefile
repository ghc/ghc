TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

WORK = work
PKGDB = $(WORK)/pkgdb
PKGA1 = $(WORK)/pkgA1
PKGA2 = $(WORK)/pkgA2
PKGB = $(WORK)/pkgB
APP = $(WORK)/app
OUT = $(WORK)/T2057.out

.PHONY: T2057 clean

clean:
	rm -rf $(WORK)

T2057: clean

	# Create an isolated package DB and output directories for the repro.
	mkdir -p '$(PKGA1)' '$(PKGA2)' '$(PKGB)' '$(APP)'
	'$(GHC_PKG)' init '$(PKGDB)'

	# Build and register pkgA from the pkgA1 sources.
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -package-db '$(PKGDB)' \
	    -this-unit-id pkgA -O -c pkgA1/A.hs -outputdir '$(PKGA1)'
	'$(AR)' q '$(PKGA1)/libHSpkgA.a' '$(PKGA1)/A.o' >/dev/null 2>&1
	cp pkgA1/pkg.conf '$(WORK)/pkgA1.conf'
	'$(GHC_PKG)' --package-db '$(PKGDB)' register '$(WORK)/pkgA1.conf' >/dev/null

	# Build and register pkgB against pkgA so INLINE g records the unfolding g = f in B.hi.
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -package-db '$(PKGDB)' \
	    -package pkgA -this-unit-id pkgB -O -c pkgB/B.hs \
	    -outputdir '$(PKGB)'
	'$(AR)' q '$(PKGB)/libHSpkgB.a' '$(PKGB)/B.o' >/dev/null 2>&1
	cp pkgB/pkg.conf '$(WORK)/pkgB.conf'
	'$(GHC_PKG)' --package-db '$(PKGDB)' register '$(WORK)/pkgB.conf' >/dev/null

	# Rebuild pkgA from the pkgA2 source tree, removing f.
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -package-db '$(PKGDB)' \
	    -this-unit-id pkgA -O -c pkgA2/A.hs -outputdir '$(PKGA2)'
	'$(AR)' q '$(PKGA2)/libHSpkgA.a' '$(PKGA2)/A.o' >/dev/null 2>&1
	cp pkgA2/pkg.conf '$(WORK)/pkgA2.conf'
	'$(GHC_PKG)' --package-db '$(PKGDB)' update '$(WORK)/pkgA2.conf' >/dev/null

	# Compiling Main against pkgB should now fail while loading the stale B.hi.
	! '$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make app/Main.hs \
	    -O -fforce-recomp -package-db '$(PKGDB)' -package pkgB \
	    >'$(OUT)' 2>&1 || { echo "expected compilation failure" >&2; exit 1; }

	# Strip the absolute test directory prefix before comparing against T2057.stderr.
	sed "s#$(CURDIR)/##g" '$(OUT)' >&2
