TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o
	rm -f *.hi
	rm -f *.dyn_o
	rm -f *.dyn_hi

T20604: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) A1 -v0
	echo "A1"
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface A1.hi | grep addDependentFile || :
	echo "A"
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface A.hi | grep addDependentFile || :
