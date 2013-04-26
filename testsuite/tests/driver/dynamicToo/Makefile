TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

TEST_HC_OPTS_DYN = -dynamic -hisuf dyn_hi -osuf dyn_o

.PHONY: dynamicToo001
# -dynamic-too should notice that the interface files for B don't match,
# and so compile the dyn way separately. This means that the right value
# of B.b should get inlined.
dynamicToo001:
	"$(TEST_HC)" $(TEST_HC_OPTS)                     -O -c A001.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) $(TEST_HC_OPTS_DYN) -O -c A001.hs
	cp B001_1.hs B001.hs
	"$(TEST_HC)" $(TEST_HC_OPTS)                     -O -c B001.hs
	cp B001_2.hs B001.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) $(TEST_HC_OPTS_DYN) -O -c B001.hs
	"$(TEST_HC)" $(TEST_HC_OPTS)                     -O -c C001.hs -dynamic-too
	"$(TEST_HC)" $(TEST_HC_OPTS) A001.o     B001.o     C001.o     -o s001
	"$(TEST_HC)" $(TEST_HC_OPTS) A001.dyn_o B001.dyn_o C001.dyn_o -o d001
	./s001
	./d001


checkExists = [ -f $1 ] || echo $1 missing

.PHONY: dynamicToo002
# Check that "--make -dynamic-too" works
dynamicToo002:
	"$(TEST_HC)" $(TEST_HC_OPTS) -dynamic-too -v0 --make C002
	$(call checkExists,A002.o)
	$(call checkExists,B002.o)
	$(call checkExists,C002.o)
	$(call checkExists,A002.hi)
	$(call checkExists,B002.hi)
	$(call checkExists,C002.hi)
	$(call checkExists,A002.dyn_o)
	$(call checkExists,B002.dyn_o)
	$(call checkExists,C002.dyn_o)
	$(call checkExists,A002.dyn_hi)
	$(call checkExists,B002.dyn_hi)
	$(call checkExists,C002.dyn_hi)

.PHONY: dynamicToo003
# Check that "-c -dynamic-too" works
dynamicToo003:
	"$(TEST_HC)" $(TEST_HC_OPTS) -dynamic-too -v0 -c A003.hs
	$(call checkExists,A003.o)
	$(call checkExists,A003.hi)
	$(call checkExists,A003.dyn_o)
	$(call checkExists,A003.dyn_hi)

