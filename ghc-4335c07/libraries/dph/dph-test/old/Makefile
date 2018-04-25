GHC = ../../../../inplace/bin/ghc-stage2

HC      = $(GHC)
HCFLAGS = -package QuickCheck -package template-haskell -package regex-compat \
          -fth -XGeneralizedNewtypeDeriving -XTypeSynonymInstances -v0
OPTFLAGS = -O2 -funbox-strict-fields -fliberate-case-threshold100
PARFLAGS = -package dph-prim-par
SEQFLAGS = -package dph-prim-seq

TESTSUITE = Testsuite/Utils.hs \
            Testsuite/Testcase.hs \
            Testsuite/Preproc.hs \
            Testsuite.hs

TESTSUITE_OBJS = $(TESTSUITE:.hs=.o)

TESTS = $(wildcard tests/*.hs)
TEST_MODS = $(notdir $(TESTS))
OPT = $(TEST_MODS:.hs=-opt)
UNOPT = $(TEST_MODS:.hs=-unopt)
# we want the tests to be run in the right order
ALL = $(TEST_MODS:.hs=-all)

TESTMAIN = 'System.Environment.withArgs (words "$(run)") main'

.PHONY: default unopt opt all testsuite

default: unopt

all: $(ALL)

unopt: $(UNOPT)

opt: $(OPT)

testsuite: $(TESTSUITE_OBJS)

Testsuite.o: $(filter-out Testsuite.o,$(TESTSUITE_OBJS))

%.o : %.hs
	$(HC) -c $< $(HCFLAGS) $(PARFLAGS)

%-opt.o: %.hs testsuite
	$(HC) -o $@ -c $< $(HCFLAGS) $(PARFLAGS) $(OPTFLAGS)

%.hi: %.o
	@:

$(TEST_OBJS) : testsuite

%-all: %-unopt %-opt
	@:

%-unopt:
	@echo "======== Testing  $(patsubst %-unopt,%,$@) (parallel, interpreted) ========"
	@$(HC) -e $(TESTMAIN) $(patsubst %-unopt,tests/%.hs,$@) $(HCFLAGS) $(PARFLAGS) \
		| tee $@.log | { grep -v '\.\.\. pass' || true; }
	@echo "======== Finished $(patsubst %-unopt,%,$@) (parallel, interpreted) ========"
	@echo "======== Testing  $(patsubst %-unopt,%,$@) (sequential, interpreted) ========"
	@$(HC) -e $(TESTMAIN) $(patsubst %-unopt,tests/%.hs,$@) $(HCFLAGS) $(SEQFLAGS) \
		| tee $@.log | { grep -v '\.\.\. pass' || true; }
	@echo "======== Finished $(patsubst %-unopt,%,$@) (sequential, interpreted) ========"

# Throws warnings
%-opt: tests/%-opt.o
	@echo "======== Testing  $(patsubst %-opt,%,$@) (optimised) ========"
	@$(HC) -o tst $(HCFLAGS) $(PARFLAGS) $< $(TESTSUITE_OBJS)
	@./tst | tee $@ | { grep -v '\.\.\. pass' || true; }
	@rm -f tst $<
	@echo "======== Finished $(patsubst %-opt,%,$@) (optimised) ========"

