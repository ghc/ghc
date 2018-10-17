TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T15038
T15038:
	'$(TEST_HC)' $(TEST_HC_OPTS)  -O2 -o Main \
	  ./test/Main.hs \
	  ./test/Parser.hs \
	  ./src/Packed/Bytes/Stream/ST.hs \
	  ./src/Packed/Bytes/Parser.hs \
	  ./src/Packed/Bytes.hs \
	  ./common/Data/Trie/Naive.hs \
		-package containers -package ghc-prim -package primitive
	./Main
