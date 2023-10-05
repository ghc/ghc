TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Test for passing -dynamic and -dynamic-too together

checkExists = [ -f $1 ] || echo $1 missing
checkNotExists = [ ! -f $1  ] || echo $1 not missing

clean:
	rm -f *.o
	rm -f *.hi
	rm -f *.dyn_o
	rm -f *.dyn_hi

T20436: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -dynamic -dynamic-too -v0 A.hs
	$(call checkExists,A.hi)
	$(call checkExists,A.o)
	$(call checkNotExists,A.dyn_hi)
	$(call checkNotExists,A.dyn_o)


