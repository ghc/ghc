TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T7835:
	$(RM) Test Test.exe Test.hi Test.o TestPrim.hi TestPrim.o test-prims.o
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make Test test-prims.cmm
	./Test

