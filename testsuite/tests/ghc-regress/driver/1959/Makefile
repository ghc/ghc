TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Test recompilation bug #1959, which failed in 6.8.1 (and earlier)

compile ::
	'$(TEST_HC)' -c -O E.hs
	'$(TEST_HC)' -c -O D.hs
	'$(TEST_HC)' -c -O -funfolding-use-threshold=0 C.hs
	'$(TEST_HC)' -c -O B.hs
	'$(TEST_HC)' E.o D.o C.o B.o -o prog

clean ::
	rm -f *.o *.hi E.hs

dotest ::
	$(MAKE) clean
	cp E1.hs E.hs
	$(MAKE) compile
	./prog
	sleep 1
	cp E2.hs E.hs
	$(MAKE) compile
	./prog
