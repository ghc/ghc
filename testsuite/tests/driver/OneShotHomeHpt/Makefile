TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Compile each module in one-shot mode. While compiling A, the interface of C
# is demand-loaded into the HPT (not the EPS), and its instances, family
# instances and rules must be visible.
OneShotHomeHpt:
	$(RM) -f *.o *.hi OneShotHomeHpt
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -O -c C.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -O -c B.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -O -c A.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 A.o B.o C.o -o OneShotHomeHpt
	./OneShotHomeHpt
