TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Remark: these should be synced up with the testsuites listed in `utils/haddock/haddock.cabal`
# See #16206


# Corresponds to files in the `haddock-test` package
haddockTest=$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock.hs \
						$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock/Config.hs \
						$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock/Utils.hs \
						$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock/Xhtml.hs \
						$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock/Process.hs

# Corresponds to the `html-test` testsuite
.PHONY: htmlTest
htmlTest:
	'$(TEST_HC)' \
		-odir . \
		-hidir . \
		-package Cabal \
		-o html-test \
		$(haddockTest) \
		$(TOP)/../utils/haddock/html-test/Main.hs
	./html-test \
		$(ACCEPT) \
		--ghc-path='$(TEST_HC)' \
		--ghc-pkg-path='$(GHC_PKG)' \
		--haddock-path='$(HADDOCK)' \

# Corresponds to the `latex-test` testsuite
.PHONY: latexTest
latexTest:
	'$(TEST_HC)' \
		-odir . \
		-hidir . \
		-package Cabal \
		-o latex-test \
		$(haddockTest) \
		$(TOP)/../utils/haddock/latex-test/Main.hs
	./latex-test \
		$(ACCEPT) \
		--ghc-path='$(TEST_HC)' \
		--ghc-pkg-path='$(GHC_PKG)' \
		--haddock-path='$(HADDOCK)' \

# Corresponds to the `hoogle-test` testsuite
.PHONY: hoogleTest
hoogleTest:
	'$(TEST_HC)' \
		-odir . \
		-hidir . \
		-package Cabal \
		-o hoogle-test \
		$(haddockTest) \
		$(TOP)/../utils/haddock/hoogle-test/Main.hs
	./hoogle-test \
		$(ACCEPT) \
		--ghc-path='$(TEST_HC)' \
		--ghc-pkg-path='$(GHC_PKG)' \
		--haddock-path='$(HADDOCK)' \

# Corresponds to the `hypsrc-test` testsuite
.PHONY: hypsrcTest
hypsrcTest:
	'$(TEST_HC)' \
		-odir . \
		-hidir . \
		-package Cabal \
		-o hypsrc-test \
		$(haddockTest) \
		$(TOP)/../utils/haddock/hypsrc-test/Main.hs
	./hypsrc-test \
		$(ACCEPT) \
		--ghc-path='$(TEST_HC)' \
		--ghc-pkg-path='$(GHC_PKG)' \
		--haddock-path='$(HADDOCK)' \

.PHONY: haddockForeignTest
haddockForeignTest:
	'$(HADDOCK)' A.hs B.hs F.hs arith.c

.PHONY: T26114
T26114:
	'$(HADDOCK)' T26114.hs

# Check that `haddock --incremental` looks for the module in the
# unit given by `-this-unit-id`.
.PHONY: haddockIncrementalUnitId
haddockIncrementalUnitId:
	rm -f IncrementalUnitId.hi IncrementalUnitId.o IncrementalUnitId.haddock IncrementalUnitId2.haddock
	'$(TEST_HC)' -c -haddock -fwrite-interface -this-unit-id mypkg-1.0 IncrementalUnitId.hs
	@echo "== haddock finds the module in the unit given by -this-unit-id =="
	'$(HADDOCK)' --incremental=IncrementalUnitId --optghc=-this-unit-id=mypkg-1.0 --dump-interface=IncrementalUnitId.haddock
