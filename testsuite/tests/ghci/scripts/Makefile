TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

ghci024:
	@echo "~~~~~~~~~~ Testing :set"
	printf ":set\n" \
	    | '$(TEST_HC)' $(TEST_HC_OPTS) --interactive -v0 -ignore-dot-ghci \
	    | grep -E "^([^ ]|  -fno-print-explicit-foralls|  -fno-warn-implicit-prelude)"
	@echo "~~~~~~~~~~ Testing :show languages"
	printf ":show languages\n" \
	    | '$(TEST_HC)' $(TEST_HC_OPTS) --interactive -v0 -ignore-dot-ghci \
	    | grep -E "^([^ ]|  -XImplicitPrelude|  -XMagicHash)"
	@echo "~~~~~~~~~~ Testing :show languages, with -XMagicHash"
	printf ":set -XMagicHash\n:show languages\n" \
	    | '$(TEST_HC)' $(TEST_HC_OPTS) --interactive -v0 -ignore-dot-ghci \
	    | grep -E "^([^ ]|  -XImplicitPrelude|  -XMagicHash)"
	@echo "~~~~~~~~~~ Testing :show packages"
	printf ":show packages\n" \
	    | '$(TEST_HC)' $(TEST_HC_OPTS) --interactive -v0 -ignore-dot-ghci \
	    | grep -E "^([^ ]|  -package|base|ghc-prim|integer|rts|Cabal)" \
	    | sed "s/Cabal-[0-9.]*/Cabal-VERSION/"
	@echo "~~~~~~~~~~ Testing :show packages, including the ghc package"
	printf ":set -package ghc\n:show packages\n" \
	    | '$(TEST_HC)' $(TEST_HC_OPTS) --interactive -v0 -ignore-dot-ghci \
	    | grep -E "^([^ ]|  -package|base|ghc-prim|integer|rts|Cabal)" \
	    | sed "s/Cabal-[0-9.]*/Cabal-VERSION/"

.PHONY: ghci037
ghci037:
	'$(TEST_HC)' $(TEST_HC_OPTS) --interactive -v0 -ignore-dot-ghci                   < ghci037.script
	'$(TEST_HC)' $(TEST_HC_OPTS) --interactive -v0 -ignore-dot-ghci -fno-ghci-sandbox < ghci037.script

