TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

ghc-e001:
	'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e "return ()"

ghc-e002:
	'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":main" ghc-e002.hs

ghc-e003:
	'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e 'putStrLn "Foo"' -e 'putStrLn "Bar"'

ghc-e004:
	'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":m + System.Exit" -e "exitWith (ExitFailure 6)"; echo $$?

# This is what runghc does:
ghc-e005:
	'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -main-is foo ghc-e005.hs -e ":set prog ghc-e005-prog" -e ":main [\"the\",\"args\"]"; echo $$?

2228:
	'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":main" 2228.hs

2636:
	'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":main" 2636.hs; if [ "$?" != 0 ]; then true; else false; fi

3890:
	'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":main" 3890.hs | cat

T7299:
	'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e "Control.Concurrent.threadDelay (1000 * 1000)"

