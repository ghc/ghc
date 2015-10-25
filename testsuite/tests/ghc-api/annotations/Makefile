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
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" AnnotationTuple

.PHONY: listcomps
listcomps:
	rm -f listcomps.o listcomps.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc listcomps
	./listcomps "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

.PHONY: T10358
T10358:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10358

.PHONY: T10396
T10396:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10396

.PHONY: T10255
T10255:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10255

.PHONY: T10357
T10357:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10357

.PHONY: T10268
T10268:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10268

.PHONY: T10280
T10280:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10280

.PHONY: T10269
T10269:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10269

.PHONY: T10312
T10312:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10312

.PHONY: T10307
T10307:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10307

.PHONY: T10309
T10309:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10309

.PHONY: boolFormula
boolFormula:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" TestBoolFormula

.PHONY: T10278
T10278:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10278

.PHONY: T10354
T10354:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10354

.PHONY: T10399
T10399:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10399

.PHONY: T10313
T10313:
	rm -f stringSource.o stringSource.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc stringSource
	./stringSource "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10313
