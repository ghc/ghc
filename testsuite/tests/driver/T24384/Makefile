TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T24384:
	./setup-dep.sh "$(TEST_HC)" "$(TEST_HC_OPTS)" "$(CABAL_MINIMAL_BUILD)" "$(GHC_PKG)"
	mkdir -p lib
	mv B.hs lib/
	'$(TEST_HC)' A.hs lib/B.hs C.hs D.hs E.hs F.hs -M -dep-json dep.json -include-pkg-deps -include-cpp-deps -package-db ./db -hide-all-packages -package base -package containers -package template-haskell -package dep -package dep:pub
	cat dep.json
