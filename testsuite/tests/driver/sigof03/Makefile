TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# -fforce-recomp makes lots of driver tests trivially pass, so we
# filter it out from $(TEST_HC_OPTS).
TEST_HC_OPTS_NO_RECOMP = $(filter-out -fforce-recomp,$(TEST_HC_OPTS))

S03_OPTS=$(TEST_HC_OPTS_NO_RECOMP) -outputdir tmp_sigof03 -i -itmp_sigof03
sigof03:
	rm -rf tmp_sigof03
	mkdir tmp_sigof03
	'$(TEST_HC)' $(S03_OPTS) -c A.hs
	'$(TEST_HC)' $(S03_OPTS) -c ASig1.hs-boot -sig-of "ASig1 is main:A, ASig2 is main:A"
	'$(TEST_HC)' $(S03_OPTS) -c ASig2.hs-boot -sig-of "ASig1 is main:A, ASig2 is main:A"
	'$(TEST_HC)' $(S03_OPTS) --merge-requirements ASig1 ASig2
	'$(TEST_HC)' $(S03_OPTS) -c Main.hs
	'$(TEST_HC)' $(S03_OPTS) tmp_sigof03/A.o tmp_sigof03/Main.o -o tmp_sigof03/Main
	./tmp_sigof03/Main

S03M_OPTS=$(TEST_HC_OPTS_NO_RECOMP) -outputdir tmp_sigof03m
sigof03m:
	rm -rf tmp_sigof03m
	mkdir tmp_sigof03m
	'$(TEST_HC)' $(S03M_OPTS) --make Main.hs -sig-of "ASig1 is main:A, ASig2 is main:A"
	./tmp_sigof03m/Main

# Currently, the type-check tests are omitted, because we don't have a
# way of telling GHC that ASig1 and ASig2 have the same identities
# (sig-of is not right because it requires the target to have an hi
# file, but in general we won't have it.)
