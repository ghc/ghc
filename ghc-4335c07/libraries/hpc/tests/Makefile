# This Makefile runs the tests using GHC's testsuite framework.  It
# assumes the package is part of a GHC build tree with the testsuite
# installed in ../../../testsuite.

TOP=../../../testsuite
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

#
# This is measuring coverage of the hpc tool itself.
#  In the hpc directory 
#   % cd $ROOT/utils/hpc
# It assumes you have make a copy of hpc that
#  is not compiled with -fhpc, and called it hpc-std
#   %  make clean ; make boot ; make ; mv hpc-inplace hpc-std
# It assumes that the utils/hpc has been compiled with 
#   % make clean ; make boot ; make UseStage1=YES EXTRA_HC_OPTS=-fhpc
#
#HPCTOOLS_BORING = HpcParser HpcOverlay HpcLexer HpcDraft HpcCombine
HPCTOOLS_BORING = HpcParser
BORING_FLAGS    = $(foreach BORING,$(HPCTOOLS_BORING),--exclude=$(BORING))
COVERAGE_OUT   = tool-coverage.info

HPC_ALL_TIX = hpc-all.tix
HPC_EXC_TIX = hpc-exceptions.tix

hpc-tool-coverage-clean::
	find . -name 'hpc.tix' -print | xargs rm -f

hpc-tool-coverage::
	$(HPC) overlay hpc.ovr --output=$(HPC_EXC_TIX)
	$(HPC) sum $(shell find . -name 'hpc-inplace.tix' -print) $(HPC_EXC_TIX) \
			--output=$(HPC_ALL_TIX)  --union
	date							    >> $(COVERAGE_OUT)
	$(HPC) report $(HPC_ALL_TIX) --xml-output    >> $(COVERAGE_OUT)
	echo OMIT: $(HPCTOOLS_BORING)				   >> $(COVERAGE_OUT)
	$(HPC) report $(HPC_ALL_TIX) --xml-output $(BORING_FLAGS) \
									>> $(COVERAGE_OUT)
	$(HPC) markup $(HPC_ALL_TIX) $(BORING_FLAGS) --destdir=html 

