TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation tests

clean:
	rm -f *.o *.hi

# bug #12723

recomp016: clean
	echo 'module B (module A) where import A' > B.hs
	echo 'first run'
	'$(TEST_HC)' $(TEST_HC_OPTS) --make E.hs
	sleep 1
	echo 'module B (module A2) where import A2' > B.hs
	echo 'second run'
	'$(TEST_HC)' $(TEST_HC_OPTS) --make E.hs
