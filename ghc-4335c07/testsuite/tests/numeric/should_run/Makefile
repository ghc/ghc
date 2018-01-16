TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T7014
T7014:
	rm -f T7014.simpl T7014.o T7014.hi
	'$(TEST_HC)' -Wall -v0 -O --make T7014.hs -fforce-recomp -ddump-simpl > T7014.simpl
	! grep -q -f T7014.primops T7014.simpl
	./T7014
