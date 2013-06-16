TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi

apirecomp001: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghciWayFlags) --make -v0 -package ghc myghc.hs
	./myghc "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

