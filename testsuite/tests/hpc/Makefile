TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Test that adding -fhpc triggers recompilation
T11798:
	"$(TEST_HC)" $(TEST_HC_ARGS) T11798
	"$(TEST_HC)" $(TEST_HC_ARGS) T11798 -fhpc
	test -e .hpc/T11798.mix

T17073:
	LANG=ASCII "$(TEST_HC)" $(TEST_HC_ARGS) T17073.hs -fhpc
	./T17073
	hpc report T17073
	LANG=ASCII hpc markup T17073
