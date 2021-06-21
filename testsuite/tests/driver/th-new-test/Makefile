TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation tests

clean:
	rm -f *.o*
	rm -f *.dyn_o*
	rm -f *.hi*
	rm -f *.dyn_hi*

th-new-test: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) --make Main.hs -fhide-source-paths -package template-haskell
	# No recompilation
	'$(TEST_HC)' $(TEST_HC_OPTS) --make Main.hs -fhide-source-paths -package template-haskell
	cp B.hs B_O.hs
	cp B1.hs B.hs
	# Recompile just B and A
	'$(TEST_HC)' $(TEST_HC_OPTS) --make Main.hs -fhide-source-paths -package template-haskell
	cp D.hs D_O.hs
	cp D1.hs D.hs
	# Recompile just D and C
	'$(TEST_HC)' $(TEST_HC_OPTS) --make Main.hs -fhide-source-paths -package template-haskell
	cp D_O.hs D.hs
	cp B_O.hs B.hs
	# Recompile A B C D
	'$(TEST_HC)' $(TEST_HC_OPTS) --make Main.hs -fhide-source-paths -package template-haskell
