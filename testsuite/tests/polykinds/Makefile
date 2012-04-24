TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# T5881 needs a script because it goes wrong only when 
# the modules are compiled separately, not with --make
T5881:
	$(RM) -f T5881.hi T5881.o T5881a.hi T5881a.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5881a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5881.hs

# T6025 is like T5881; needs separat compile
T6025:
	$(RM) -f T6025.hi T6025.o T6025a.hi T6025a.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T6025a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T6025.hs

