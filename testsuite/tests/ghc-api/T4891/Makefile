TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi

T4891: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc T4891
	./T4891 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

.PHONY: clean T4891

