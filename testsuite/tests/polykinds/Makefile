TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# T5881 needs a script because it goes wrong only when 
# the modules are compiled separately, not with --make
T5881:
	$(RM) -f T5881.hi T5881.o T5881a.hi T5881a.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5881a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5881.hs

# T6025 is like T5881; needs separate compile
T6025:
	$(RM) -f T6025.hi T6025.o T6025a.hi T6025a.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T6025a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T6025.hs

# T6054 is like T5881; needs separate compile
# The second compile fails, and should do so, hence leading "-"
T6054:
	$(RM) -f T6054.hi T6054.o T6054a.hi T6054a.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T6054a.hs
	-'$(TEST_HC)' $(TEST_HC_OPTS) -c T6054.hs

T7022:
	$(RM) -f T7022.hi T7022.o T7022a.hi T7022a.o T7022b.hi T7022b.o
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) -c T7022a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) -c T7022b.hs -v0
	-'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) -c -v0 T7022.hs

T7438:
	$(RM) -f T7438.hi T7438.o T7438a.hi T7438a.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T7438a.hs
	-'$(TEST_HC)' $(TEST_HC_OPTS) -c T7438.hs

# Need to compile separately so that the interface files get read
T8449:
	$(RM) -f T8449.hi T8449.o T8449a.hi T8449a.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T8449a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T8449.hs
