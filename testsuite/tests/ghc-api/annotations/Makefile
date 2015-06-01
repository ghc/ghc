TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi
	rm -f annotations comments parseTree exampleTest
	rm -f listcomps
	rm -f boolFormula
	rm -f t10255
	rm -f t10268
	rm -f t10269
	rm -f t10278
	rm -f t10280
	rm -f t10307
	rm -f t10309
	rm -f t10312
	rm -f t10354
	rm -f t10357
	rm -f t10358
	rm -f t10396
	rm -f t10399

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
	rm -f exampleTest.o exampleTest.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc \
		-outputdir tmp_exampleTest \
    exampleTest
	./exampleTest "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" AnnotationTuple

.PHONY: listcomps
listcomps:
	rm -f listcomps.o listcomps.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc listcomps
	./listcomps "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

.PHONY: T10358
T10358:
	rm -f t10358.o t10358.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc  \
		-outputdir tmp_T10358 \
    t10358
	./t10358 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10358

.PHONY: T10396
T10396:
	rm -f t10396.o t10396.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc  \
		-outputdir tmp_T10396 \
    t10396
	./t10396 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10396

.PHONY: T10255
T10255:
	rm -f t10255.o t10255.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc  \
		-outputdir tmp_T10255 \
    t10255
	./t10255 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10255

.PHONY: T10357
T10357:
	rm -f t10357.o t10357.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc  \
		-outputdir tmp_T10357 \
    t10357
	./t10357 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10357

.PHONY: T10268
T10268:
	rm -f t10268.o t10268.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc  \
		-outputdir tmp_T10268 \
    t10268
	./t10268 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10268

.PHONY: T10280
T10280:
	rm -f t10280.o t10280.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc  \
		-outputdir tmp_T10280 \
    t10280
	./t10280 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10280

.PHONY: T10269
T10269:
	rm -f t10269.o t10269.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc  \
		-outputdir tmp_T10269 \
    t10269
	./t10269 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10269

.PHONY: T10312
T10312:
	rm -f t10312.o t10312.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc  \
		-outputdir tmp_T10312 \
    t10312
	./t10312 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10312

.PHONY: T10307
T10307:
	rm -f t10307.o t10307.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc  \
		-outputdir tmp_T10307 \
    t10307
	./t10307 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10307

.PHONY: T10309
T10309:
	rm -f t10309.o t10309.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc  \
		-outputdir tmp_T10309 \
    t10309
	./t10309 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10309

.PHONY: boolFormula
boolFormula:
	rm -f boolFormula.o boolFormula.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc  \
		-outputdir tmp_boolFormula \
    boolFormula
	./boolFormula "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" TestBoolFormula

.PHONY: T10278
T10278:
	rm -f t10278.o t10278.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc  \
		-outputdir tmp_T10278 \
    t10278
	./t10278 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10278

.PHONY: T10354
T10354:
	rm -f t10354.o t10354.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc  \
		-outputdir tmp_T10354 \
    t10354
	./t10354 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10354

.PHONY: T10399
T10399:
	rm -f t10399.o t10399.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc  \
		-outputdir tmp_T10399 \
    t10399
	./t10399 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10399

.PHONY: T10313
T10313:
	rm -f stringSource.o stringSource.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc stringSource
	./stringSource "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Test10313
