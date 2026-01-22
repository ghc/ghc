TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

hpc002:
	HPCTIXFILE=ghc.tix "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) $(TEST_HC_ARGS) hpc002.hs -fhpc -v0 -e ":main"
	"$(HPC)" report ghc
	"$(HPC)" report ghc --per-module
	LANG=ASCII "$(HPC)" markup ghc

hpc003:
	HPCTIXFILE=ghc.tix "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) $(TEST_HC_ARGS) -fhpc -v0 < hpc003.script
	"$(HPC)" report ghc || echo "Can't summarise file that is ':load'ed"	"$(HPC)" report ghc
	"$(HPC)" report ghc --per-module
	"$(HPC)" markup ghc
