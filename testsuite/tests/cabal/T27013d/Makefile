TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Test that compiling Data.Composition with -hide-all-packages and
# -XNoImplicitPrelude does not recompile the second or third time. In !15899,
# it used to recompile because of an incorrect "GHC.Essentials package changed".
#
# We pass -frebindable-known-names so GHC doesn't force an implicit
# GHC.Essentials dependency: despite this module not using known-key names,
#  -hide-all-packages means we can't find GHC.Essentials when it is added as an
#  implicit dependency.
clean:
	rm -f *.o *.hi

T27013d: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) Composition.hs -XNoImplicitPrelude -hide-all-packages -frebindable-known-names
	'$(TEST_HC)' $(TEST_HC_OPTS) Composition.hs -XNoImplicitPrelude -hide-all-packages -frebindable-known-names
	'$(TEST_HC)' $(TEST_HC_OPTS) Composition.hs -XNoImplicitPrelude -hide-all-packages -frebindable-known-names
