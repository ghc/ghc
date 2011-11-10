TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation tests

clean:
	rm -f *.o *.hi
	rm -f Test$(exeext)
	rm -f Test2$(exeext)

# bug #437

437: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make -main-is Test.main Test.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make -main-is Test2.main Test2.hs
	./Test
	./Test2

