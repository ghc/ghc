TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

STACK_SELECTOR := 'Info\(53\): [0-9]+'
FALLBACK_SELECTOR := 'Tables with fallback: [0-9]+'
SKIPPED_SELECTOR := 'Tables skipped: [0-9]+'

# This test runs ghc on the Fold.hs file four times with the following sets of
# flags:
# 1. -finfo-table-map
# 2. -finfo-table-map -fno-info-table-map-with-stack
# 3. -finfo-table-map -fno-info-table-map-with-fallback
# 4. -finfo-table-map -fno-info-table-map-with-stack -fno-info-table-map-with-fallback
#
# From (1), we collect baseline statistics about how many fallback and stack
# info tables there are and make sure that 0 tables are skipped by default. From
# (2), we determine how many stack info tables get skipped with
# -fno-info-table-with-stack. From (3), we determine the same for fallback
# tables. From (4) we determine how many total are skipped by both flags. The
# last command in the chain below is what checks for consistency among these
# statistics.

ipe_stats:
	@NoOmit="$$('$(TEST_HC)' $(TEST_HC_OPTS) -fforce-recomp -finfo-table-map -dipe-stats Fold.hs)" ; \
	Stack=$$(echo $$NoOmit | grep -Eo $(STACK_SELECTOR) | grep -Eo '[0-9]+$$') ; \
	Fallback=$$(echo $$NoOmit | grep -Eo $(FALLBACK_SELECTOR) | grep -Eo '[0-9]+$$') ; \
	Skipped=$$(echo $$NoOmit | grep -Eo $(SKIPPED_SELECTOR) | grep -Eo '[0-9]+$$') ; \
	Both=$$(expr $$Stack + $$Fallback) ; \
	OmitStack="$$('$(TEST_HC)' $(TEST_HC_OPTS) -fforce-recomp -finfo-table-map -fno-info-table-map-with-stack -dipe-stats Fold.hs)" ; \
	SkippedStack=$$(echo $$OmitStack | grep -Eo $(SKIPPED_SELECTOR) | grep -Eo '[0-9]+$$') ; \
	OmitFallback="$$('$(TEST_HC)' $(TEST_HC_OPTS) -fforce-recomp -finfo-table-map -fno-info-table-map-with-fallback -dipe-stats Fold.hs)" ; \
	SkippedFallback=$$(echo $$OmitFallback| grep -Eo $(SKIPPED_SELECTOR) | grep -Eo '[0-9]+$$') ; \
	OmitBoth="$$('$(TEST_HC)' $(TEST_HC_OPTS) -fforce-recomp -finfo-table-map -fno-info-table-map-with-stack -fno-info-table-map-with-fallback -dipe-stats Fold.hs)" ; \
	SkippedBoth=$$(echo $$OmitBoth| grep -Eo $(SKIPPED_SELECTOR) | grep -Eo '[0-9]+$$') ; \
	echo "Baseline number of skipped tables: $$Skipped\n" ; \
	echo "Baseline number of stack tables:                                  $$Stack" ; \
	echo "Number of omitted stack tables (only -fno-info-table-with-stack): $$SkippedStack\n" ; \
	echo "Baseline number of fallback tables:                                      $$Fallback" ; \
	echo "Number of omitted fallback tables (only -fno-info-table-with-fallback):  $$SkippedFallback\n" ; \
	echo "Baseline number of both tables (just a sum of both baselines):                                                $$Both" ; \
	echo "Number of omitted stack and fallback tables (both -fno-info-table-with-stack -fno-info-table-with-fallback):  $$SkippedBoth\n" ; \
	echo "Baseline skipped must be 0 and all pairs of stats above must match for this test to pass!" ; \
	[ "$$Skipped" = "0" ] && [ "$$Stack" = "$$SkippedStack" ] && [ "$$Fallback" = "$$SkippedFallback" ] && [ "$$Both" = "$$SkippedBoth" ]
