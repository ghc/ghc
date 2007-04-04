TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# We don't want make to fail here, because it emits an unpredictable
# error message.  Instead we really want to test the exit code of GHC,
# hence the 
#    echo "exit($$?)" 1>&2
# which emits GHC's exit code to stderr so the test driver can compare it.

mod144::
	rm -f Mod143_A.o Mod143_A.hi mod144.o mod144.hi mod144
	$(TEST_HC) -c Mod143_A.hs
	$(TEST_HC) -c mod144.hs || echo "exit($$?)" 1>&2

mod146::
	rm -f Mod145_A.o Mod145_A.hi mod146.o mod146.hi mod146
	$(TEST_HC) -c Mod145_A.hs
	$(TEST_HC) -c mod146.hs || echo "exit($$?)" 1>&2

mod158::
	rm -f Mod157_*.o
	rm -f Mod157_*.hi
	rm -f mod158.o mod158.hi mod158
	$(TEST_HC) -c Mod157_A.hs
	$(TEST_HC) -c Mod157_B.hs
	$(TEST_HC) -c Mod157_C.hs
	$(TEST_HC) -c Mod157_D.hs
	$(TEST_HC) -c mod158.hs || echo "exit($$?)" 1>&2

mod160::
	rm -f Mod159_*.o
	rm -f Mod159_*.hi
	rm -f mod160.o mod160.hi mod160
	$(TEST_HC) -c Mod159_A.hs
	$(TEST_HC) -c Mod159_B.hs
	$(TEST_HC) -c Mod159_C.hs
	$(TEST_HC) -c Mod159_D.hs
	$(TEST_HC) -c mod160.hs || echo "exit($$?)" 1>&2

mod165::
	rm -f Mod164_*.o
	rm -f Mod164_*.hi
	rm -f mod165.o mod165.hi mod165
	$(TEST_HC) -c Mod164_A.hs
	$(TEST_HC) -c Mod164_B.hs
	$(TEST_HC) -c mod165.hs || echo "exit($$?)" 1>&2

mod166::
	rm -f Mod164_*.o
	rm -f Mod164_*.hi
	rm -f mod166.o mod166.hi mod166
	$(TEST_HC) -c Mod164_A.hs
	$(TEST_HC) -c Mod164_B.hs
	$(TEST_HC) -c mod166.hs || echo "exit($$?)" 1>&2

mod167::
	rm -f Mod164_*.o
	rm -f Mod164_*.hi
	rm -f mod167.o mod167.hi mod167
	$(TEST_HC) -c Mod164_A.hs
	$(TEST_HC) -c Mod164_B.hs
	$(TEST_HC) -c mod167.hs || echo "exit($$?)" 1>&2

