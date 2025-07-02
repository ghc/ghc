TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi

T26154:
	$(MAKE) -s --no-print-directory clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -c B.hs-boot
	'$(TEST_HC)' $(TEST_HC_OPTS) -c A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Other.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T26154.hs
