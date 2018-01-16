TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation tests

clean:
	rm -f *.o *.hi

# bug #8247

recomp013: clean
	echo 'module A where a1 = 5; a2 = 42; a3 = 113' > A.hs
	echo 'module B (module A) where import A hiding (a1)' > B.hs
	echo 'module C where import B; a2 = 142' > C.hs
	echo 'first run'
	'$(TEST_HC)' $(TEST_HC_OPTS) --make C.hs
	sleep 1
	echo 'module B (module A) where import A hiding (a1, a2)' > B.hs
	echo 'second run'
	'$(TEST_HC)' $(TEST_HC_OPTS) --make C.hs
