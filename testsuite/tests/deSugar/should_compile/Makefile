TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T5252:
	$(RM) -f T5252.hi  T5252.o
	$(RM) -f T5252a.hi T5252a.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5252a.hs 
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5252.hs

# Failed when compiled *without* optimisation
T5252Take2:
	$(RM) -f T5252Take2.hi  T5252Take2.o
	$(RM) -f T5252Take2a.hi T5252Take2a.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5252Take2a.hs 
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5252Take2.hs

T10844:
	$(RM) -f T10844.hi  T10844.o
	$(RM) -f T10844a.hi T10844a.o
	# check that the string "foo" appears in the simplified core
	# of T10844a, but *not* in T10844
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c T10844a.hs -ddump-simpl | grep '"foo"' || true
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c T10844.hs  -ddump-simpl | grep '"foo"' || true
