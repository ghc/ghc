TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

LIBDIR := "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

# Calculate the number of module dependencies of 'Parser.' If that
# number exceeds a threshold, that indicates that the dependencies
# have significantly gone up via the commit under test (and the test
# is deemed to fail). In that case, this most likely means a cycle
# has arisen that pulls in modules for Core generation. The
# motivation for not allowing that to happen is so that the
# 'ghc-lib-parser' package subset of the GHC API can continue to be
# provided with as small a number of modules as possible for when the
# need exists to produce ASTs and nothing more.

.PHONY: count-deps-parser
count-deps-parser:
	$(COUNT_DEPS) $(LIBDIR) "GHC.Parser" | tee out
	cat out | tail -n +2 | wc -l > SIZE

.PHONY: count-deps-ast
count-deps-ast:
	$(COUNT_DEPS) $(LIBDIR) "Language.Haskell.Syntax" | tee out
	cat out | tail -n +2 | wc -l > SIZE
