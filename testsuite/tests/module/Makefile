TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# We don't want make to fail here, because it emits an unpredictable
# error message.  Instead we really want to test the exit code of GHC,
# hence the 
#    echo "exit($$?)" 1>&2
# which emits GHC's exit code to stderr so the test driver can compare it.

mod144::
	rm -f Mod144_A.o Mod144_A.hi mod144.o mod144.hi mod144
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Mod144_A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c mod144.hs || echo "exit($$?)" 1>&2

# These tests share some source files, so to avoid clashing when
# using multiple threads we direct the outputs to different files
# depending on the test name.
AUX_O  = $@_o
AUX_HI = $@_hi
AUX_OPTS = -osuf $(AUX_O) -hisuf $(AUX_HI)

mod146::
	rm -f Mod145_A.$(AUX_O) Mod145_A.$(AUX_HI) mod146.$(AUX_O) mod146.$(AUX_HI) mod146
	'$(TEST_HC)' $(TEST_HC_OPTS) $(AUX_OPTS) -c Mod145_A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) $(AUX_OPTS) -c mod146.hs || echo "exit($$?)" 1>&2

mod158::
	rm -f Mod157_*.$(AUX_O) Mod157_*.$(AUX_HI)
	rm -f mod158.$(AUX_O) mod158.$(AUX_HI) mod158
	'$(TEST_HC)' $(TEST_HC_OPTS) $(AUX_OPTS) -c Mod157_A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) $(AUX_OPTS) -c Mod157_B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) $(AUX_OPTS) -c Mod157_C.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) $(AUX_OPTS) -c Mod157_D.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) $(AUX_OPTS) -c mod158.hs || echo "exit($$?)" 1>&2

mod160::
	rm -f Mod159_*.$(AUX_O)
	rm -f Mod159_*.$(AUX_HI)
	rm -f mod160.$(AUX_O) mod160.$(AUX_HI) mod160
	'$(TEST_HC)' $(TEST_HC_OPTS) $(AUX_OPTS) -c Mod159_A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) $(AUX_OPTS) -c Mod159_B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) $(AUX_OPTS) -c Mod159_C.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) $(AUX_OPTS) -c Mod159_D.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) $(AUX_OPTS) -c mod160.hs || echo "exit($$?)" 1>&2

mod165::
	rm -f Mod164_*.$(AUX_O)
	rm -f Mod164_*.$(AUX_HI)
	rm -f mod165.$(AUX_O) mod165.$(AUX_HI) mod165
	'$(TEST_HC)' $(TEST_HC_OPTS) $(AUX_OPTS) -c Mod164_A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) $(AUX_OPTS) -c Mod164_B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) $(AUX_OPTS) -c mod165.hs || echo "exit($$?)" 1>&2

mod166::
	rm -f Mod164_*.$(AUX_O)
	rm -f Mod164_*.$(AUX_HI)
	rm -f mod166.$(AUX_O) mod166.$(AUX_HI) mod166
	'$(TEST_HC)' $(TEST_HC_OPTS) $(AUX_OPTS) -c Mod164_A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) $(AUX_OPTS) -c Mod164_B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) $(AUX_OPTS) -c mod166.hs || echo "exit($$?)" 1>&2

mod167::
	rm -f Mod164_*.$(AUX_O)
	rm -f Mod164_*.$(AUX_HI)
	rm -f mod167.$(AUX_O) mod167.$(AUX_HI) mod167
	'$(TEST_HC)' $(TEST_HC_OPTS) $(AUX_OPTS) -c Mod164_A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) $(AUX_OPTS) -c Mod164_B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) $(AUX_OPTS) -c mod167.hs || echo "exit($$?)" 1>&2

