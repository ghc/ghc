TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T1133 T11148

T11148:
	$(RM) T11148.hi T11148.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -dunique-increment=-1 -dinitial-unique=16777215 T11148.hs
	# Set the unique-increment to '-1' to trigger the bug
	# This has to be done on the command line;
	#  an OPTIONS_GHC pragma does not do it

T1133:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T1133.hs-boot
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T1133a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T1133.hs

