TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation tests for .hie files

clean:
	rm -f *.o *.hi *.hie

recomp018: clean
	echo 'first run'
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -fwrite-ide-info A.hs
	sleep 1

	# Check the .hie files have been created
	test -f A.hie
	test -f B.hie
	test -f C.hie

	# Remove some of those files
	rm A.hie B.hie

	# Recompile
	echo 'second run'
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -fwrite-ide-info A.hs

	# Check the .hie files have been created
	test -f A.hie
	test -f B.hie
	test -f C.hie
