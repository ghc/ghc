TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: sum_api_annots
sum_api_annots:
	number=1 ; while [[ $$number -le 11 ]] ; do \
	  $(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" unboxedsums$$number.hs ; \
		((number = number + 1)) ; \
	done

unpack_sums_7:
	$(RM) -f unpack_sums_7.o unpack_sums_7.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c unpack_sums_7.hs  -O -dsuppress-all -ddump-simpl | grep -q '\(#|_#\)'
	# This is a test to check for the presence of an unboxed sum in the core for a program using UNPACK
	# on a sum type which is evidence that the field has been correctly unpacked.
