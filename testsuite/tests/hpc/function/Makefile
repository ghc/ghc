TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

tough1:
	HPCTIXFILE=ghc.tix "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTICE) -v0 -fhpc tough.hs -e "main"
	"$(HPC)" report ghc.tix
	"$(HPC)" report ghc.tix --per-module
	"$(HPC)" markup ghc.tix
