TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi

T6145:
	rm -f T6145.o T6145.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc T6145
	./T6145 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

T8639_api:
	rm -f T8639_api.o T8639_api.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc T8639_api
	./T8639_api "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

T8628:
	rm -f T8628.o T8628.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc -package exceptions T8628
	./T8628 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

T9015:
	rm -f T9015.o T9015.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc T9015
	./T9015 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

.PHONY: clean T6145 T8639_api T8628 T9015


