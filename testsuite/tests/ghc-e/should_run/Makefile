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

ghc-e006:
	'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e "data X = X deriving Show" -e "X"

T2228:
	'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":main" T2228.hs </dev/null

T2636:
	'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":main" T2636.hs; if [ "$?" != 0 ]; then true; else false; fi

T3890:
	'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":main" T3890.hs | cat

T7299:
	'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e "Control.Concurrent.threadDelay (1000 * 1000)"

T9086:
	'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":main" T9086.hs

T9905:
	'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e "import Data.List (sort)" -e "sort [2,1]"

T9905b:
	'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e "import qualified Data.List as L" -e "L.sort [2,1]"

# Ensure that ghc -e does not output extra messages about GHCi configuration files
T11478:
	'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -ghci-script T11478.script -e 3
