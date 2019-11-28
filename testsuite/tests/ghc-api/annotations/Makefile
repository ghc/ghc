TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi
	rm -f annotations comments parseTree
	rm -f listcomps
	rm -f stringSource
	rm -f t11430

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

.PHONY: T10358
T10358:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10358.hs

.PHONY: T10396
T10396:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10396.hs

.PHONY: T10255
T10255:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10255.hs

.PHONY: T10357
T10357:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10357.hs

.PHONY: T10268
T10268:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10268.hs

.PHONY: T10280
T10280:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10280.hs

.PHONY: T10269
T10269:
	# Ignore result code, we have an unattached (superfluous) AnnVal
	- $(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10269.hs

.PHONY: T10312
T10312:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10312.hs

.PHONY: T10307
T10307:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10307.hs

.PHONY: T10309
T10309:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10309.hs

.PHONY: boolFormula
boolFormula:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" TestBoolFormula.hs

.PHONY: T10278
T10278:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10278.hs

.PHONY: T10354
T10354:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10354.hs

.PHONY: T10399
T10399:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10399.hs

.PHONY: bundle-export
bundle-export:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" BundleExport.hs

.PHONY: T10313
T10313:
	rm -f stringSource.o stringSource.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc stringSource
	./stringSource "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10313

.PHONY: T11018
T11018:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test11018.hs

.PHONY: T10276
T10276:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10276.hs

.PHONY: T10598
T10598:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10598.hs

.PHONY: T11321
T11321:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test11321.hs

.PHONY: T11332
T11332:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test11332.hs

.PHONY: T11430
T11430:
	rm -f t11430.o t11430.hi t11430
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc t11430
	./t11430 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test11430

.PHONY: load-main
load-main:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" load-main.hs

.PHONY: T12417
T12417:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test12417.hs

.PHONY: T13163
T13163:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test13163.hs

.PHONY: T15303
T15303:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test15303.hs

.PHONY: T16212
T16212:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test16212.hs

.PHONY: T16230
T16230:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test16230.hs

.PHONY: T16236
T16236:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test16236.hs

.PHONY: StarBinderAnns
StarBinderAnns:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" StarBinderAnns.hs

.PHONY: T16279
T16279:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test16279.hs

.PHONY: T17388
T17388:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test17388.hs

.PHONY: T17519
T17519:
	$(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test17519.hs
