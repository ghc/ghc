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

T20348A: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -o A2.o -ohi A2.hi -dynohi A2.dyn_hi A.hs -dynamic-too
	$(call checkExists,A2.hi)
	$(call checkExists,A2.o)
	$(call checkExists,A2.dyn_hi)
	$(call checkExists,A2.dyn_o)

T20348B: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -o A2.o -ohi A2.hi A.hs -dynamic-too
	$(call checkExists,A2.hi)
	$(call checkExists,A2.o)
	$(call checkExists,A.dyn_hi)
	$(call checkExists,A2.dyn_o)

T20348C: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -o A2.o -dyno A2.dyn_o A.hs -dynamic-too
	$(call checkExists,A.hi)
	$(call checkExists,A2.o)
	$(call checkExists,A.dyn_hi)
	$(call checkExists,A2.dyn_o)

T20348D: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -dyno A2.dyn_o A.hs -dynamic-too
	$(call checkExists,A.hi)
	$(call checkExists,A.o)
	$(call checkExists,A.dyn_hi)
	$(call checkExists,A2.dyn_o)

T20348E: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -S -o A2.s A.hs -dynamic-too
	$(call checkExists,A2.s)
	$(call checkExists,A2.dyn_s)


