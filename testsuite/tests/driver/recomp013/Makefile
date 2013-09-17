TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# -fforce-recomp makes lots of driver tests trivially pass, so we
# filter it out from $(TEST_HC_OPTS).
TEST_HC_OPTS_NO_RECOMP = $(filter-out -fforce-recomp,$(TEST_HC_OPTS))

# Recompilation tests

clean:
	rm -f *.o *.hi

# bug #8247

recomp013: clean
	echo 'module A where a1 = 5; a2 = 42; a3 = 113' > A.hs
	echo 'module B (module A) where import A hiding (a1)' > B.hs
	echo 'module C where import B; a2 = 142' > C.hs
	echo 'first run'
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) --make C.hs
	echo 'module B (module A) where import A hiding (a1, a2)' > B.hs
	echo 'second run'
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) --make C.hs
