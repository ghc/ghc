TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f A/Setup A/Setup.o A/Setup.hi
	rm -f B/Setup B/Setup.o B/Setup.hi
	rm -rf A/dist B/dist
	rm -f package.conf

T3007:
	$(MAKE) clean
	echo '[]' > package.conf
	cd A && '$(TEST_HC)' -v0 --make Setup
	cd A && ./Setup configure -v0 --with-compiler='$(TEST_HC)' --ghc-pkg-option=--global-package-db=../package.conf --ghc-option=-package-db../package.conf
	cd A && ./Setup build -v0
	cd A && ./Setup register --inplace -v0
	cd B && '$(TEST_HC)' -v0 --make Setup
	cd B && ./Setup configure -v0 --with-compiler='$(TEST_HC)' --ghc-pkg-option=--global-package-db=../package.conf --ghc-option=-package-db../package.conf
	cd B && ./Setup build -v0

