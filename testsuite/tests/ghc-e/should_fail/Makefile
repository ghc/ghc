TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T7962:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e "return (" || echo $$? >&2

T9905fail1:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e "import This.Module.Does.Not.Exist"  || echo $$? >&2

T9905fail2:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e "import Data.List (bogusIdentifier)"  || echo $$? >&2

T9905fail3:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e "import Prelude (+)" || echo $$? >&2 # syntax error

ghc-e-fail1:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e "class ["  || echo $$? >&2

ghc-e-fail2:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e "type A = A"  || echo $$? >&2

T9930fail:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -x hs T9930

T18441fail0:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":" || echo $$? >&2

T18441fail1:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":abcde" || echo $$? >&2

T18441fail2:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":set -Xabcde" || echo $$? >&2 # unrecognized flag

T18441fail4:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":load Abcde" || echo $$? >&2 # no module

T18441fail5:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":reload Abcde" || echo $$? >&2 # no module

T18441fail6:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":add Abcde" || echo $$? >&2 # no module

T18441fail7:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":script Abcde" || echo $$? >&2 # no script

T18441fail8:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":script Abc def" || echo $$? >&2 # bad script input

T18441fail9:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":def abc" || echo $$? >&2 # macro not an expr

T18441fail10:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":def :abc" || echo $$? >&2 # bad macro

T18441fail11:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":def !abc" || echo $$? >&2 # bad macro

T18441fail12:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":cmd abc" || echo $$? >&2 # cmd compilation failure

T18441fail13:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":t" || echo $$? >&2

T18441fail14:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":type-at H.hs 1 1 1 1 f" || echo $$? >&2

T18441fail15:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":loc-at H.hs 1 1 1 1 f" || echo $$? >&2

T18441fail16:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":k" || echo $$? >&2

T18441fail17:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":run a" || echo $$? >&2

T18441fail18:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e ":main" || echo $$? >&2
