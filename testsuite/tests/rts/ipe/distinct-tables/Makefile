TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# This test runs ghc with various combinations of
# -f{no-}distinct-constructor-tables for different constructors and checks that
# whereFrom finds (or fails to find) their provenance appropriately.

distinct_tables:
	@$$TEST_HC $$TEST_HC_OPTS -finfo-table-map -fdistinct-constructor-tables=ACon Main.hs ; \
	ACon="$$(./Main)" ; \
	$$TEST_HC $$TEST_HC_OPTS -finfo-table-map -fdistinct-constructor-tables=BCon Main.hs ; \
	BCon="$$(./Main)" ; \
	$$TEST_HC $$TEST_HC_OPTS -finfo-table-map -fdistinct-constructor-tables=CCon Main.hs ; \
	CCon="$$(./Main)" ; \
	$$TEST_HC $$TEST_HC_OPTS -finfo-table-map -fdistinct-constructor-tables=ACon,BCon Main.hs ; \
	AConBCon="$$(./Main)" ; \
	$$TEST_HC $$TEST_HC_OPTS -finfo-table-map -fdistinct-constructor-tables -fno-distinct-constructor-tables=ACon Main.hs ; \
	NoACon="$$(./Main)" ; \
	$$TEST_HC $$TEST_HC_OPTS -finfo-table-map -fdistinct-constructor-tables -fno-distinct-constructor-tables=BCon Main.hs ; \
	NoBCon="$$(./Main)" ; \
	$$TEST_HC $$TEST_HC_OPTS -finfo-table-map -fdistinct-constructor-tables -fno-distinct-constructor-tables=CCon Main.hs ; \
	NoCCon="$$(./Main)" ; \
	$$TEST_HC $$TEST_HC_OPTS -finfo-table-map -fdistinct-constructor-tables -fno-distinct-constructor-tables=BCon,CCon Main.hs ; \
	NoBConCCon="$$(./Main)" ; \
	echo "$$ACon" | diff --strip-trailing-cr ACon.out - && \
	echo "$$BCon" | diff --strip-trailing-cr BCon.out - && \
	echo "$$CCon" | diff --strip-trailing-cr CCon.out - && \
	echo "$$AConBCon" | diff --strip-trailing-cr AConBCon.out - && \
	echo "$$NoACon" | diff --strip-trailing-cr NoACon.out - && \
	echo "$$NoBCon" | diff --strip-trailing-cr NoBCon.out - && \
	echo "$$NoCCon" | diff --strip-trailing-cr NoCCon.out - && \
	echo "$$NoBConCCon" | diff --strip-trailing-cr NoBConCCon.out -
