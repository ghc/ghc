TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation tests

clean:
	rm -f *.o *.hi

# bug #13099

recomp017: clean
	echo 'first run'
	'$(TEST_HC)' $(TEST_HC_OPTS) --make D.hs
	sleep 1
	# A simple 'cp' would overwrite the original file,
	# pointed to by the symlink
	mv -f C2.hs C.hs
	touch C.hs
	echo 'second run'
	! '$(TEST_HC)' $(TEST_HC_OPTS) --make E.hs
