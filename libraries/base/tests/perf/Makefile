# This Makefile runs the tests using GHC's testsuite framework.  It
# assumes the package is part of a GHC build tree with the testsuite
# installed in ../../../testsuite.

TOP=../../../../testsuite
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk


T17752:
	'$(TEST_HC)' $(TEST_HC_OPTS) -O --make T17752 -rtsopts -ddump-simpl -ddump-to-file -dsuppress-uniques
	# We only grep for specific patterns to minimize failures when eg core syntax changes
	# We expect two unfolds using eqChar
	echo $$(cat T17752.dump-simpl | grep unpackFoldrCString)
	echo $$(cat T17752.dump-simpl | grep eqChar)
	# And two pattern matches
	echo $$(cat T17752.dump-simpl | grep "case x1" -A4 )
