#################################################################################
#
#			target.mk
#
#		ghc/tests/programs standard target rules
#
#################################################################################

# Link step
$(TEST_PROG) : $(OBJS)
	$(HC) $(HC_OPTS) $(OBJS) -o $(TEST_PROG)

# Run test
runtest : $(TEST_PROG)
	$(RUNTEST) ./$< \
	  $(addprefix -i ,$(wildcard $(TEST_PROG).stdin)) \
	  $(addprefix -o1 ,$(wildcard $(TEST_PROG).stdout)) \
	  $(addprefix -o2 ,$(wildcard $(TEST_PROG).stderr)) \
	  $(RUNTEST_OPTS)

# Include standard boilerplate
# We do this at the end for cosmetic reasons: it means that the "normal-way"
# runtests will precede the "other-way" recursive invocations of make

include $(FPTOOLS_TOP)/mk/target.mk

