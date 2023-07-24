TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# This test runs ghc with various combinations of
# -f{no-}distinct-constructor-tables for different constructors and checks that
# whereFrom finds (or fails to find) their provenance appropriately.

distinct_tables01:
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -finfo-table-map -fdistinct-constructor-tables-only=ACon Main.hs
	./Main

distinct_tables02:
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -finfo-table-map -fdistinct-constructor-tables-only=BCon Main.hs
	./Main

distinct_tables03:
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -finfo-table-map -fdistinct-constructor-tables-only=CCon Main.hs
	./Main

distinct_tables04:
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -finfo-table-map -fdistinct-constructor-tables-only=ACon,BCon Main.hs
	./Main

distinct_tables05:
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -finfo-table-map -fdistinct-constructor-tables-only=ACon -fdistinct-constructor-tables-only=BCon -fdistinct-constructor-tables-only=CCon Main.hs
	./Main

distinct_tables06:
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -finfo-table-map -fdistinct-constructor-tables-only=ACon -fno-distinct-constructor-tables Main.hs
	./Main

distinct_tables07:
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -finfo-table-map -fdistinct-constructor-tables -fdistinct-constructor-tables-only=ACon -fno-distinct-constructor-tables Main.hs
	./Main

distinct_tables08:
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -finfo-table-map -fno-distinct-constructor-tables Main.hs
	./Main

distinct_tables09:
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -finfo-table-map -fno-distinct-constructor-tables -fdistinct-constructor-tables-only=ACon Main.hs
	./Main

distinct_tables10:
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -finfo-table-map -fdistinct-constructor-tables Main.hs
	./Main

# Recompilation tests
distinct_tables11:
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -finfo-table-map -fdistinct-constructor-tables Main.hs
	./Main
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -finfo-table-map -fdistinct-constructor-tables -fdistinct-constructor-tables-only=ACon Main.hs
	./Main

distinct_tables12:
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -finfo-table-map -fno-distinct-constructor-tables Main.hs
	./Main
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -finfo-table-map -fdistinct-constructor-tables Main.hs
	./Main

distinct_tables13:
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -finfo-table-map -fdistinct-constructor-tables Main.hs
	./Main
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -finfo-table-map -fdistinct-constructor-tables -fdistinct-constructor-tables-only=ACon Main.hs
	./Main
