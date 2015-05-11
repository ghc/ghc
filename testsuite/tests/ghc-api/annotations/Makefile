TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi
	rm -f annotations comments parseTree exampleTest
	rm -f t10269
	rm -f t10255 t10312
	rm -f t1037
	rm -f t10309
	rm -f listcomps boolFormula

annotations: 
	rm -f annotations.o annotations.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc annotations
	./annotations "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

parseTree: 
	rm -f parseTree.o parseTree.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc parseTree
	./parseTree "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

comments: 
	rm -f comments.o comments.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc comments
	./comments "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

exampleTest:
	rm -f exampleTest.o exampleTest.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc exampleTest
	./exampleTest "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

listcomps:
	rm -f listcomps.o listcomps.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc listcomps
	./listcomps "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

t10255:
	rm -f t10255.o t10255.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc t10255
	./t10255 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

.PHONY: clean annotations parseTree comments exampleTest listcomps t10255

T10268:
	rm -f t10268.o t10268.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc t10268
	./t10268 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

T10280:
	rm -f t10280.o t10280.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc t10280
	./t10280 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

.PHONY: clean annotations parseTree comments exampleTest listcomps

T10269:
	rm -f T10269.o T10269.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc t10269
	./t10269 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

.PHONY: t10269

t10312:
	rm -f t10312.o t10312.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc t10312
	./t10312 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

.PHONY: clean annotations parseTree comments exampleTest listcomps t10255 t10312

t10307:
	rm -f t10307.o t10307.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc t10307
	./t10307 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

.PHONY: t10307

t10309:
	rm -f t10309.o t10309.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc t10309
	./t10309 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

.PHONY: t10309

boolFormula:
	rm -f boolFormula.o boolFormula.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc boolFormula
	./boolFormula "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

.PHONY: clean annotations parseTree comments exampleTest listcomps boolFormula
