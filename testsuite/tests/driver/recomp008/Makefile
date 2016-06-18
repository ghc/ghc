TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

Main.o : B.hi
B.o : A.hi

%.o : %.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c $< -o $@

# magic .hi rule: see rules/hi-rule.mk in the GHC source tree
%.hi : %.o ;

prog : A.o B.o Main.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -o prog A.o B.o Main.o

clean :
	$(RM) *.hi *.o prog

recomp008 : 
	$(MAKE) -s --no-print-directory clean
	cp A1.hs A.hs
	$(MAKE) -s --no-print-directory prog
	./prog
	sleep 1
	cp A2.hs A.hs
	$(MAKE) -s --no-print-directory prog
	./prog
