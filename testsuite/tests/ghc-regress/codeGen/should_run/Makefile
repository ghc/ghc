TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: 5149
5149:
	$(RM) 5149_cmm.o 5149.o 5149.hi 5149$(exeext)
	"$(TEST_HC)" -v0 -O -c 5149_cmm.cmm
	"$(TEST_HC)" -v0 --make -O 5149.hs 5149_cmm.o -o 5149$(exeext)
	./5149
