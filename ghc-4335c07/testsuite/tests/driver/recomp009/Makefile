TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation tests

clean:
	rm -f *.o *.hi
	rm -f B.hs C
	rm -f Sub.hs
	rm -f Main$(exeext)

# bug #481

recomp009: clean
	cp Sub1.hs Sub.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) -v0 --make Main.hs
	./Main
	sleep 1
	cp Sub2.hs Sub.hs
	-'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) -v0 --make Main.hs
	./Main
