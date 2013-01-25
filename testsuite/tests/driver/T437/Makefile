TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# -fforce-recomp makes lots of driver tests trivially pass, so we
# filter it out from $(TEST_HC_OPTS).
TEST_HC_OPTS_NO_RECOMP = $(filter-out -fforce-recomp,$(TEST_HC_OPTS))

# Recompilation tests

clean:
	rm -f *.o *.hi
	rm -f Test$(exeext)
	rm -f Test2$(exeext)

# bug #437

T437: clean
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP)  --make -main-is Test.main Test.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP)  --make -main-is Test2.main Test2.hs
	./Test
	./Test2
	sleep 1
	# Test that changing the main function name forces recomp of Test2
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP)  --make -main-is Test2.doit Test2.hs
	./Test2

