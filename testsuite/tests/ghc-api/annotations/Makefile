TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi
	rm -f annotations comments
	rm -f stringSource

.PHONY: annotations
annotations:
	rm -f annotations.o annotations.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc annotations
	./annotations "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

.PHONY: comments
comments:
	rm -f comments.o comments.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc comments
	./comments "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

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
