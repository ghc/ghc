TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f A/Setup A/Setup.o A/Setup.hi
	rm -f B/Setup B/Setup.o B/Setup.hi
	rm -rf A/dist B/dist
	rm -rf package.conf

# --no-user-package-db to avoid warning about missing settings file

T3007:
	$(MAKE) -s --no-print-directory clean
	'$(GHC_PKG)' init package.conf
	cd A && '$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Setup
	cd A && ./Setup configure -v0 --with-compiler='$(TEST_HC)' --ghc-pkg-option=--global-package-db=../package.conf --ghc-pkg-option=--no-user-package-db --ghc-option=-package-db../package.conf
	cd A && ./Setup build -v0
	cd A && ./Setup register --inplace -v0
	cd B && '$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Setup
	cd B && ./Setup configure -v0 --with-compiler='$(TEST_HC)' --ghc-pkg-option=--global-package-db=../package.conf --ghc-pkg-option=--no-user-package-db --ghc-option=-package-db../package.conf
	cd B && ./Setup build -v0

