TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi
	rm -f annotations comments parseTree exampleTest

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

T10278:
	rm -f t10278.o t10278.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc t10278
	./t10278 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

.PHONY: clean annotations parseTree comments exampleTest listcomps
