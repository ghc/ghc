TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi
	rm -f annotations comments parseTree
	rm -f listcomps
	rm -f stringSource

.PHONY: annotations
annotations:
	rm -f annotations.o annotations.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc annotations
	./annotations "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

.PHONY: parseTree
parseTree:
	rm -f parseTree.o parseTree.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc parseTree
	./parseTree "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

.PHONY: comments
comments:
	rm -f comments.o comments.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc comments
	./comments "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

.PHONY: exampleTest
exampleTest:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" AnnotationTuple.hs

.PHONY: listcomps
listcomps:
	rm -f listcomps.o listcomps.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc listcomps
	./listcomps "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

.PHONY: boolFormula
boolFormula:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" TestBoolFormula.hs

.PHONY: bundle-export
bundle-export:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" BundleExport.hs

# .PHONY: T10313
# T10313:
# 	rm -f stringSource.o stringSource.hi
# 	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc stringSource
# 	./stringSource "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10313

# .PHONY: T11430
# T11430:
# 	rm -f t11430.o t11430.hi t11430
# 	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc t11430
# 	./t11430 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test11430

.PHONY: load-main
load-main:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" load-main.hs
