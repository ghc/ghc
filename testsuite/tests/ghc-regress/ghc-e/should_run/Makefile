TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

ghc-e001:
	$(TEST_HC) -ignore-dot-ghci -e "return ()"

ghc-e002:
	$(TEST_HC) -ignore-dot-ghci -e ":main" ghc-e002.hs

ghc-e003:
	$(TEST_HC) -ignore-dot-ghci -e "putStrLn \"Foo\"" -e "putStrLn \"Bar\""

ghc-e004:
	$(TEST_HC) -ignore-dot-ghci -e ":m + System.Exit" -e "exitWith (ExitFailure 6)"; echo $$?

