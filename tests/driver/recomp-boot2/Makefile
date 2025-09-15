TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation tests

clean:
	rm -f *.o *.hi
	rm -f B.hs

# Recompile, adding the extra boot dependency should also cause Top to be recompiled
# even though we don't use the boot file.

recomp-boot2: clean
	cp B1.hs B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make Top.hs
	sleep 1
	cp B2.hs B.hs
	# Operating systems with poor timer resolution (e.g. Darwin) need a bit
	# of time here, lest GHC not realize that the file changed.
	'$(TEST_HC)' $(TEST_HC_OPTS) --make Top.hs
