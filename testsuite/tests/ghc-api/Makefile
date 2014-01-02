TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi

T6145: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc T6145
	./T6145 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

T8639_api: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc T8639_api
	./T8639_api "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

.PHONY: clean T6145


