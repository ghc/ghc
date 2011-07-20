TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

DECIMAL_REGEXP = [0-9]\+.[0-9]\+

define scc001Test
	grep -q '^ *$1 \+Main \+[0-9]\+ \+1 \+$(DECIMAL_REGEXP) \+$(DECIMAL_REGEXP) \+$(DECIMAL_REGEXP) \+$(DECIMAL_REGEXP)$$' scc001.prof || { echo 'No single-entry for $1 found in profile'; grep '^ *$1 ' scc001.prof; true; }
endef

define scc001Rule
	rm -f scc001.hi scc001.o scc001
	@echo Compiling with $1
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make scc001 -prof -auto-all -rtsopts $1
	./scc001 +RTS -p
	$(call scc001Test,f)
	$(call scc001Test,g)
	$(call scc001Test,h)
endef

.PHONY: scc001
scc001:
	$(call scc001Rule,-O0)
	$(call scc001Rule,-O)

