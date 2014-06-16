TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# -fforce-recomp makes lots of driver tests trivially pass, so we
# filter it out from $(TEST_HC_OPTS).
TEST_HC_OPTS_NO_RECOMP = $(filter-out -fforce-recomp,$(TEST_HC_OPTS))

OBJSUFFIX = .o

# Test that adding a new module that shadows a package module causes
# recompilation.  Part of bug #1372.
recomp003:
	$(RM) A.hi A$(OBJSUFFIX) out
	$(RM) -rf Data
	mkdir Data
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -c A.hs
	echo "module Data.Char where" > Data/Char.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -c Data/Char.hs
	# Should now recompile A.hs, because Char is now a home module:
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -c A.hs 2>err
	@if grep NOT err; then false; fi
