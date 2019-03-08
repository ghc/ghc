TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP=./Setup -v0
CONFIGURE=$(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=tmp.d --prefix='$(PWD)/inst'

bkpcabal01: clean
	$(MAKE) -s --no-print-directory clean
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Setup
	cp p/P.hs.in1 p/P.hs
	cp q/Q.hs.in1 q/Q.hs
	# typecheck p
	$(CONFIGURE) --cid "p-0.1" p
	$(SETUP) build
	$(SETUP) copy
	$(SETUP) register
	# build impl
	$(CONFIGURE) --cid "impl-0.1" impl
	$(SETUP) build
	$(SETUP) copy
	$(SETUP) register
	# build p
	$(CONFIGURE) --cid "p-0.1" p --instantiate-with "H=impl-0.1:H"
	$(SETUP) build
	$(SETUP) copy
	$(SETUP) register
	# typecheck q
	$(CONFIGURE) --cid "q-0.1" q
	$(SETUP) build
	$(SETUP) copy
	$(SETUP) register
	# build q
	$(CONFIGURE) --cid "q-0.1" q --instantiate-with "I=impl-0.1:I"
	$(SETUP) build
	$(SETUP) copy
	$(SETUP) register
	# OK, now the crux of the test: recompilation.
	cp p/P.hs.in2 p/P.hs
	cp q/Q.hs.in2 q/Q.hs
	# re-typecheck p
	$(CONFIGURE) --cid "p-0.1" p
	$(SETUP) build
	$(SETUP) copy
	$(SETUP) register
	# re-build p
	$(CONFIGURE) --cid "p-0.1" p --instantiate-with "H=impl-0.1:H"
	$(SETUP) build
	$(SETUP) copy
	$(SETUP) register
	# re-typecheck q (if buggy, this is what would fail)
	$(CONFIGURE) --cid "q-0.1" q
	$(SETUP) build
	$(SETUP) copy
	$(SETUP) register
	# re-build q
	$(CONFIGURE) --cid "q-0.1" q --instantiate-with "I=impl-0.1:I"
	$(SETUP) build
	$(SETUP) copy
	$(SETUP) register
	# build exe
	$(CONFIGURE) --cid "exe-0.1" exe
	$(SETUP) build
	dist/build/exe/exe
ifneq "$(CLEANUP)" ""
	$(MAKE) -s --no-print-directory clean
endif

clean :
	$(RM) -r tmp.d inst dist Setup$(exeext)
