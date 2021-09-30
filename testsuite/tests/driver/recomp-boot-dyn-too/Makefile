TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation tests

clean:
	rm -f *.dyn_hi *.dyn_hi-boot *.dyn_o *.dyn_o-boot *.hi *.hi-boot *.o *.o-boot
	rm -f B.hs

# Recompile

recomp-boot-dyn-too: clean
	cp B1.hs B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -dynamic-too B.hs
	cp B2.hs B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -dynamic-too B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -dynamic-too B.hs
