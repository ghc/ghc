TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: unpack_sums_7

unpack_sums_7:
	$(RM) -f unpack_sums_7.o unpack_sums_7.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c unpack_sums_7.hs  -O -dsuppress-all -dsuppress-uniques -ddump-simpl | grep '\(# |_ #\)'
	# This is a test to check for the presence of an unboxed sum in the core for a program using UNPACK
	# on a sum type which is evidence that the field has been correctly unpacked.
