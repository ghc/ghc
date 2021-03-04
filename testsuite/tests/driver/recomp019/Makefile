TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation test for when .hi files are up to date but .o files are
# not

clean:
	rm -f Main *.o *.hi B.hs

recomp019: clean
	cp B1.hs B.hs
	echo 'first run'
	'$(TEST_HC)' $(TEST_HC_OPTS) --make Main
	./Main
	
	# Update B.hs
	cp B2.hs B.hs
	
	# Update B.hi (but not B.o)
	'$(TEST_HC)' $(TEST_HC_OPTS) --make B -fno-code -fwrite-interface
	
	# Recompile
	echo 'second run'
	'$(TEST_HC)' $(TEST_HC_OPTS) --make Main
	./Main
