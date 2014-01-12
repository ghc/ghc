TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# -fforce-recomp makes lots of driver tests trivially pass, so we
# filter it out from $(TEST_HC_OPTS).
TEST_HC_OPTS_NO_RECOMP = $(filter-out -fforce-recomp,$(TEST_HC_OPTS))

Main.o : B.hi
B.o : A.hi

%.o : %.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -O -c $< -o $@

# magic .hi rule: see rules/hi-rule.mk in the GHC source tree
%.hi : %.o ;

prog : A.o B.o Main.o
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -o prog A.o B.o Main.o

clean :
	$(RM) *.hi *.o prog

recomp008 : 
	$(MAKE) clean
	cp A1.hs A.hs
	$(MAKE) prog
	./prog
	sleep 1
	cp A2.hs A.hs
	$(MAKE) prog
	./prog
