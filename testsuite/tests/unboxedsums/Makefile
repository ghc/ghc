TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: sum_api_annots
sum_api_annots:
	number=1 ; while [[ $$number -le 11 ]] ; do \
	  $(CHECK_API_ANNOTATIONS) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`" unboxedsums$$number.hs ; \
		((number = number + 1)) ; \
	done
