TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Test recompilation bug #1959, which failed in 6.8.1 (and earlier)

compile ::
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O E.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O D.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O -funfolding-use-threshold=0 C.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) E.o D.o C.o B.o -o prog

clean ::
	rm -f *.o *.hi E.hs

dotest ::
	$(MAKE) -s --no-print-directory clean
	cp E1.hs E.hs
	$(MAKE) -s --no-print-directory compile
	./prog
	sleep 1
	cp E2.hs E.hs
	$(MAKE) -s --no-print-directory compile
	./prog
