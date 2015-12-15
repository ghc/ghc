TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# T10482
#   The intent here is to check that $wfoo has type
#         $wfoo :: Int# -> Int# -> Int
#   with two unboxed args.  See Trac #10482 for background
T10482:
	$(RM) -f T10482.o T10482.hi
	# Set -dppr-cols to ensure output doesn't wrap
	'$(TEST_HC)' $(TEST_HC_OPTS) -dppr-cols=200 -O -c -ddump-simpl T10482.hs | grep 'T10482.*wfoo.*Int'

T10482a:
	$(RM) -f T10482a.o T10482a.hi
	# Set -dppr-cols to ensure output doesn't wrap
	'$(TEST_HC)' $(TEST_HC_OPTS) -dppr-cols=200 -O -c -ddump-simpl T10482a.hs | grep 'wf.*Int'

T10694:
	$(RM) -f T10694.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c -ddump-simpl T10694.hs | grep 'DmdType '
