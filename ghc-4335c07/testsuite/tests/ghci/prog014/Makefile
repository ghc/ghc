TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi

prog014: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -c dummy.c

.PHONY: clean prog014
