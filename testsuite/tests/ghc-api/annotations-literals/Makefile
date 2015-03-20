TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi

literals:
	rm -f literals.o literals.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc literals
	./literals "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

parsed:
	rm -f parsed.o parsed.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc parsed
	./parsed "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

.PHONY: clean literals parsed
