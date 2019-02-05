TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Remark: these should be synced up with the testsuites listed in `utils/haddock/haddock.cabal`
# See #16206

.PHONY: htmlTest
htmlTest:
	'$(TEST_HC)' \
		-odir . \
		-package Cabal \
		-o html-test \
		$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock.hs \
		$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock/Config.hs \
		$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock/Utils.hs \
		$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock/Xhtml.hs \
		$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock/Process.hs \
		$(TOP)/../utils/haddock/html-test/Main.hs
	./html-test \
		--ghc-path=$(TEST_HC) \
		--haddock-path=$(HADDOCK) \
		--haddock-stdout=haddock-out.log &> html-test-out.log
	! grep 'FAIL' html-test-out.log

.PHONY: latexTest
latexTest:
	'$(TEST_HC)' \
		-odir . \
		-package Cabal \
		-o latex-test \
		$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock.hs \
		$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock/Config.hs \
		$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock/Utils.hs \
		$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock/Xhtml.hs \
		$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock/Process.hs \
		$(TOP)/../utils/haddock/latex-test/Main.hs
	./latex-test \
		--ghc-path=$(TEST_HC) \
		--haddock-path=$(HADDOCK) \
		--haddock-stdout=haddock-out.log &> latex-test-out.log
	! grep 'FAIL' latex-test-out.log

.PHONY: hoogleTest
hoogleTest:
	'$(TEST_HC)' \
		-odir . \
		-package Cabal \
		-o hoogle-test \
		$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock.hs \
		$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock/Config.hs \
		$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock/Utils.hs \
		$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock/Xhtml.hs \
		$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock/Process.hs \
		$(TOP)/../utils/haddock/hoogle-test/Main.hs
	./hoogle-test \
		--ghc-path=$(TEST_HC) \
		--haddock-path=$(HADDOCK) \
		--haddock-stdout=haddock-out.log &> hoogle-test-out.log
	! grep 'FAIL' hoogle-test-out.log

.PHONY: hypsrcTest
hypsrcTest:
	'$(TEST_HC)' \
		-odir . \
		-package Cabal \
		-o hypsrc-test \
		$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock.hs \
		$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock/Config.hs \
		$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock/Utils.hs \
		$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock/Xhtml.hs \
		$(TOP)/../utils/haddock/haddock-test/src/Test/Haddock/Process.hs \
		$(TOP)/../utils/haddock/hypsrc-test/Main.hs
	./hypsrc-test \
		--ghc-path=$(TEST_HC) \
		--haddock-path=$(HADDOCK) \
		--haddock-stdout=haddock-out.log &> hypsrc-test-out.log
	! grep 'FAIL' hypsrc-test-out.log
