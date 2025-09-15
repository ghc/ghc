# Tests for recompilation involving changes in export lists

TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: clean

clean:
	rm -f *.o *.hi *.o-boot *.hi-boot

# Test for #25881: don't recompile if we have an explicit import list
# and an irrelevant item is added to the module we are importing from.
RecompExports1:

	cp RecompExports1_M.hs_1 RecompExports1_M.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports1_M.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports1_N.hs

  # Add a bunch of things we don't import; should not recompile
	cp RecompExports1_M.hs_2 RecompExports1_M.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports1_M.hs
	echo "RecompExports1: N should not recompile"
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports1_N.hs

  # Remove something we explicitly imported; should recompile and fail
	cp RecompExports1_M.hs_3 RecompExports1_M.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports1_M.hs
	echo "RecompExports1: N should recompile and fail"
	! '$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports1_N.hs

  # Remove the things we were't importing; should not recompile
	cp RecompExports1_M.hs_1 RecompExports1_M.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports1_M.hs
	echo "RecompExports1: N should not recompile"
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports1_N.hs

# Like 'RecompExports1', except that we don't have an explicit import list
# and thus we must pessimistically recompile
RecompExports2:

	cp RecompExports2_M.hs_1 RecompExports2_M.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports2_M.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports2_N.hs

	cp RecompExports2_M.hs_2 RecompExports2_M.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports2_M.hs
	echo "RecompExports2: N should recompile and succeed"
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports2_N.hs

	cp RecompExports2_M.hs_3 RecompExports2_M.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports2_M.hs
	echo "RecompExports2: N should recompile and fail"
	! '$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports2_N.hs

# Like 'RecompExports1', except that we have an import list which implicitly
# imports an item, using T(..) syntax. So we must pessimistically recompile.
RecompExports3:

	cp RecompExports3_M.hs_1 RecompExports3_M.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports3_M.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports3_N.hs

	cp RecompExports3_M.hs_2 RecompExports3_M.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports3_M.hs
	echo "RecompExports3: N should recompile and fail"
	! '$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports3_N.hs

	cp RecompExports3_M.hs_3 RecompExports3_M.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports3_M.hs
	echo "RecompExports3: N should recompile and succeed"
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports3_N.hs

# Check that we enforce the right Avail structure, e.g. if we are importing
# Cls(meth), check that the module we are importing does bundle meth with Cls.
RecompExports4:

	cp RecompExports4_M.hs_1 RecompExports4_M.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports4_M.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports4_N.hs

	cp RecompExports4_M.hs_2 RecompExports4_M.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports4_M.hs
	echo "RecompExports4: N should recompile and fail"
	! '$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports4_N.hs

# Check that we recompile when we add an orphan instance, even if the importing
# module has an explicit import list.
RecompExports5:

	cp RecompExports5_M.hs_1 RecompExports5_M.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports5_M.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports5_N.hs

	cp RecompExports5_M.hs_2 RecompExports5_M.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports5_M.hs
	echo "RecompExports4: N should recompile and succeed"
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExports5_N.hs
