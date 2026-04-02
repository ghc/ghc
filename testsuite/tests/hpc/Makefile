TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Test that adding -fhpc triggers recompilation
T11798:
	"$(TEST_HC)" $(TEST_HC_ARGS) T11798
	"$(TEST_HC)" $(TEST_HC_ARGS) T11798 -fhpc
	test -e .hpc/T11798.mix

T17073a:
	LANG=ASCII "$(TEST_HC)" $(TEST_HC_ARGS) T17073.hs -fhpc -v0
	./T17073
	"$(HPC)" report T17073
	"$(HPC)" version
	LANG=ASCII "$(HPC)" markup T17073

T17073b:
	HPCTIXFILE=ghc.tix "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) $(TEST_HC_ARGS) T17073.hs -fhpc -v0 -e ":main"
	"$(HPC)" report ghc
	"$(HPC)" version
	LANG=ASCII "$(HPC)" markup ghc

T20568a:
	"$(TEST_HC)" $(TEST_HC_ARGS) T20568.hs -fhpc -v0
	./T20568

T20568b:
	HPCTIXFILE=ghc.tix "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) $(TEST_HC_ARGS) T20568.hs -fhpc -v0 -e ":main"
