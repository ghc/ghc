TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONEY: foo

foo:
	echo hello

T13821A:
	$(RM) -f T13821A.hi-boot T13821A.o-boot
	!('$(TEST_HC)' $(TEST_HC_OPTS) -c T13821A.hs-boot)
