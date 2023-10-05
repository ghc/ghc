TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation tests

clean:
	rm -f *.o *.hi
	rm -f B.hs

# Recompile

recomp-boot: clean
	cp B1.hs B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make A.hs
	sleep 1
	cp B2.hs B.hs
	# Operating systems with poor timer resolution (e.g. Darwin) need a bit
	# of time here, lest GHC not realize that the file changed.
	'$(TEST_HC)' $(TEST_HC_OPTS) --make A.hs
