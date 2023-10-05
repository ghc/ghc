TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP=../Setup -v0
CONFIGURE=$(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --with-hc-pkg='$(GHC_PKG)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d --prefix='$(PWD)/../dist'

T22333: clean
	$(MAKE) -s --no-print-directory clean
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Setup
	# build b
	cd my-package-b; $(CONFIGURE) --cid "my-package-b-0.1" my-package-b
	cd my-package-b; $(SETUP) build
	cd my-package-b; $(SETUP) copy
	cd my-package-b; $(SETUP) register
	# build a
	cd my-package-a; $(CONFIGURE) --cid "my-package-a-0.1" my-package-a
	cd my-package-a; $(SETUP) build
	cd my-package-a; $(SETUP) copy
	cd my-package-a; $(SETUP) register
	# build my-exe
	cd my-exe; $(CONFIGURE) --cid "my-exe-0.1" my-exe
	cd my-exe; $(SETUP) build
	cd my-exe; $(SETUP) copy
	cd my-exe; $(SETUP) register
ifneq "$(CLEANUP)" ""
	$(MAKE) -s --no-print-directory clean
endif

clean :
	$(RM) -r tmp.d dist Setup$(exeext)
