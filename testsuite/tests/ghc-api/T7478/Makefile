TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi

T7478: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc T7478.hs
	./T7478 "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

