TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi

T10052: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc T10052
	./T10052 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" -XScopedTypeVariables -O

.PHONY: clean T10052
