TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

OBJSUFFIX = .o

# Test that adding a new module that shadows a package module causes
# recompilation.  Part of bug #1372.
recomp003:
	$(RM) A.hi A$(OBJSUFFIX) out
	$(RM) -rf Data
	mkdir Data
	'$(TEST_HC)' $(TEST_HC_OPTS) -c A.hs
	echo "module Data.Char where" > Data/Char.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Data/Char.hs
	# Should now recompile A.hs, because Char is now a home module:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c A.hs 2>err
	@if grep NOT err; then false; fi
