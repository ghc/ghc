TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

mod144::
	rm -f Mod143_A.o Mod143_A.hi mod144.o mod144.hi mod144
	@$(TEST_HC) -c Mod143_A.hs
	@$(TEST_HC) mod144.hs

mod146::
	rm -f Mod145_A.o Mod145_A.hi mod146.o mod146.hi mod146
	@$(TEST_HC) -c Mod145_A.hs
	@$(TEST_HC) mod146.hs

mod158::
	rm -f Mod157_*.o
	rm -f Mod157_*.hi
	rm -f mod158.o mod158.hi mod158
	$(TEST_HC) -c Mod157_A.hs
	$(TEST_HC) -c Mod157_B.hs
	$(TEST_HC) -c Mod157_C.hs
	$(TEST_HC) -c Mod157_D.hs
	$(TEST_HC) mod158.hs -o mod158

