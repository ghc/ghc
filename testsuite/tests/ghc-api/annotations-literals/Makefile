TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi

literals: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc literals
	./literals "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

parsed: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc parsed
	./parsed "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

.PHONY: clean
