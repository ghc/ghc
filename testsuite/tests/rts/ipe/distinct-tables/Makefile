TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# This test runs ghc with various combinations of
# -f{no-}distinct-constructor-tables for different constructors and checks that
# whereFrom finds (or fails to find) their provenance appropriately.

distinct_tables01:
	@$$TEST_HC $$TEST_HC_OPTS -v0 -finfo-table-map -fdistinct-constructor-tables=ACon Main.hs && ./Main

distinct_tables02:
	@$$TEST_HC $$TEST_HC_OPTS -v0 -finfo-table-map -fdistinct-constructor-tables=BCon Main.hs && ./Main

distinct_tables03:
	@$$TEST_HC $$TEST_HC_OPTS -v0 -finfo-table-map -fdistinct-constructor-tables=CCon Main.hs && ./Main

distinct_tables04:
	@$$TEST_HC $$TEST_HC_OPTS -v0 -finfo-table-map -fdistinct-constructor-tables=ACon,BCon Main.hs && ./Main

distinct_tables05:
	@$$TEST_HC $$TEST_HC_OPTS -v0 -finfo-table-map -fdistinct-constructor-tables -fno-distinct-constructor-tables=ACon Main.hs && ./Main

distinct_tables06:
	@$$TEST_HC $$TEST_HC_OPTS -v0 -finfo-table-map -fdistinct-constructor-tables -fno-distinct-constructor-tables=BCon Main.hs && ./Main

distinct_tables07:
	@$$TEST_HC $$TEST_HC_OPTS -v0 -finfo-table-map -fdistinct-constructor-tables -fno-distinct-constructor-tables=CCon Main.hs && ./Main

distinct_tables08:
	@$$TEST_HC $$TEST_HC_OPTS -v0 -finfo-table-map -fdistinct-constructor-tables -fno-distinct-constructor-tables=BCon,CCon Main.hs && ./Main
