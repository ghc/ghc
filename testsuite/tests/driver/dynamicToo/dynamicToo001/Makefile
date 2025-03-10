TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

TEST_HC_OPTS_DYN = -dynamic -hisuf dyn_hi -osuf dyn_o

checkExists = [ -f $1 ] || echo $1 missing

.PHONY: dynamicToo001
# -dynamic-too should notice that the interface files for B don't match,
#  so an error is issued.
#
#  In the past this used to compile the dyn way separately. This means that the "right" value
#  of B.b should get inlined. (See #20446 for reasons why this was removed).
dynamicToo001:
	"$(TEST_HC)" $(TEST_HC_OPTS)                     -O -c A.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) $(TEST_HC_OPTS_DYN) -O -c A.hs
	cp B1.hs B.hs
	"$(TEST_HC)" $(TEST_HC_OPTS)                     -O -c B.hs
	cp B2.hs B.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) $(TEST_HC_OPTS_DYN) -O -c B.hs
	"$(TEST_HC)" $(TEST_HC_OPTS)                     -O -c C.hs -dynamic-too || true
#	"$(TEST_HC)" $(TEST_HC_OPTS) A.o     B.o     C.o     -o s
#	"$(TEST_HC)" $(TEST_HC_OPTS) A.dyn_o B.dyn_o C.dyn_o -o d
#	./s
#	./d


# Missing a .dyn_hi file and using --make mode
.PHONY: dynamicToo001MakeA
dynamicToo001MakeA:
	cp B1.hs B.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -O C.hs -dynamic-too
	rm B.dyn_hi
	# Should recompile
	"$(TEST_HC)" $(TEST_HC_OPTS) -O C.hs -dynamic-too
	$(call checkExists,B.dyn_hi)

# Should notice that B.hi and B.dyn_hi are different, and recompile B
.PHONY: dynamicToo001MakeB
dynamicToo001MakeB:
	"$(TEST_HC)" $(TEST_HC_OPTS)                     -O -c A.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) $(TEST_HC_OPTS_DYN) -O -c A.hs
	cp B1.hs B.hs
	"$(TEST_HC)" $(TEST_HC_OPTS)                     -O -c B.hs
	cp B2.hs B.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) $(TEST_HC_OPTS_DYN) -O -c B.hs
	cp B1.hs B.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -O  C.hs -dynamic-too

.PHONY: T25837
# Test that -dynamic-too notices when interface files were compiled with different flags
T25837:
	# First compile without -dynamic but with -O0
	"$(TEST_HC)" $(TEST_HC_OPTS) -O0 -c T25837Module.hs

	# Then compile with -dynamic but with -ticky
	"$(TEST_HC)" $(TEST_HC_OPTS) $(TEST_HC_OPTS_DYN) -ticky -c T25837Module.hs

	# Now try to compile with -dynamic-too, should detect the mismatch and recompile
	"$(TEST_HC)" $(TEST_HC_OPTS) -v1 -dynamic-too --make T25837Module.hs

	# Verify that both interfaces now match (optional check)
	$(call checkExists,T25837Module.hi)
	$(call checkExists,T25837Module.dyn_hi)


