TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T24384:
	mkdir -p lib
	mv B.hs lib/
	'$(TEST_HC)' -M A.hs lib/B.hs C.hs D.hs E.hs -dep-json dep.json -include-pkg-deps -include-cpp-deps -package containers -package template-haskell
	cat dep.json
