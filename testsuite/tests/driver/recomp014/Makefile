TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# -fforce-recomp makes lots of driver tests trivially pass, so we
# filter it out from $(TEST_HC_OPTS).
TEST_HC_OPTS_NO_RECOMP = $(filter-out -fforce-recomp,$(TEST_HC_OPTS))

# Recompilation tests

clean:
	rm -f *.o *.hi

recomp014: clean
	echo 'module A where a = False' > A.hs
	echo 'module A1 where a = False' > A1.hs
	echo 'module B where a :: Bool' > B.hs-boot
	echo 'first run'
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -c A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -c A1.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -c B.hs-boot -sig-of "B is main:A"
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) --merge-requirements B
	echo 'import B; main = print a' > C.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -c C.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -c C.hs
	echo 'second run'
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -c B.hs-boot -sig-of "B is main:A1"
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) --merge-requirements B
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -c C.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) A1.o C.o -o recomp014
	./recomp014

.PHONY: clean recomp014
