TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi *.ppr.hs
	rm Ppr003 Ppr004

.PHONY: ppr001
ppr001:
	$(CHECK_PPR) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Ppr001.hs

.PHONY: ppr002
ppr002:
	$(CHECK_PPR) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Ppr002.hs

.PHONY: ppr003
ppr003:
	$(CHECK_PPR) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Ppr003.hs

.PHONY: ppr004
ppr004:
	$(CHECK_PPR) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Ppr004.hs

.PHONY: ppr005
ppr005:
	$(CHECK_PPR) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Ppr005.hs

.PHONY: ppr006
ppr006:
	$(CHECK_PPR) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Ppr006.hs

.PHONY: ppr007
ppr007:
	$(CHECK_PPR) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" Ppr007.hs
