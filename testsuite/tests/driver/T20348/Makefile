TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation test for -fno-code -fwrite-interface with TemplateHaskell.

checkExists = [ -f $1 ] || echo $1 missing

clean:
	rm -f *.o
	rm -f *.hi
	rm -f *.dyn_o
	rm -f *.dyn_hi

T20348: clean
	# First run: should produce .hi, .o, .dyn_hi, .dyn_o files.
	echo 'first run'
	'$(TEST_HC)' $(TEST_HC_OPTS) -fno-code -fwrite-interface A.hs
	$(call checkExists,A.hi)
	$(call checkExists,A.o)
	$(call checkExists,A.dyn_hi)
	$(call checkExists,A.dyn_o)

	# Second run: should not recompile.
	echo 'second run'
	'$(TEST_HC)' $(TEST_HC_OPTS) -fno-code -fwrite-interface A.hs
