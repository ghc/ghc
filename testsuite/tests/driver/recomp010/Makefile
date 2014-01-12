TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# -fforce-recomp makes lots of driver tests trivially pass, so we
# filter it out from $(TEST_HC_OPTS).
TEST_HC_OPTS_NO_RECOMP = $(filter-out -fforce-recomp,$(TEST_HC_OPTS))

# Recompilation tests

clean:
	rm -f *.o *.hi
	rm -f X.hs
	rm -f Main$(exeext)

# bug #5614

recomp010: clean
	cp X1.hs X.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -v0 --make -O Main.hs
	./Main
	sleep 1
	cp X2.hs X.hs
	rm X.o
	-'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -v0 --make -O Main.hs
	./Main
