TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Test that threadDelay can be interrupted by ^C.
# Wait for the statement to start before interrupting (#15383), and signal
# the external interpreter too (the `pgrep -P $!`), as ^C in a terminal would.
#
# The ext interp tests both:
# 	- interrupting a remote request doesn't terminate the ext interpreter (#18852)
# 	- a ^C gets propagated to the thread evaluating the user request on the ext
# 	interpreter so that it gets interrupted
T3171ext: T3171_OPTS = -fexternal-interpreter
T3171 T3171ext:
	rm -f $@.started
	echo 'do { writeFile "$@.started" ""; (Control.Concurrent.threadDelay 100000000 >> putStrLn "threadDelay was not interrupted.") `Control.Exception.catch` \(e :: Control.Exception.SomeException) -> System.IO.hPutStrLn System.IO.stderr ("threadDelay was interrupted: " ++ show e) >> Control.Exception.throwIO e }' | \
	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) $(T3171_OPTS) & \
	while [ ! -e $@.started ] && kill -0 $$! 2>/dev/null; do sleep 0.1; done; \
	kill -INT $$! $$(pgrep -P $$!); wait

# Like T3171, but ^C interrupts a :force rather than an expression: the
# computation being forced must be interrupted too, not left running.
ForceInterruptExt: ForceInterrupt_OPTS = -fexternal-interpreter
ForceInterrupt ForceInterruptExt:
	rm -f force.started
	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) $(ForceInterrupt_OPTS) < ForceInterrupt.script & \
	while [ ! -e force.started ] && kill -0 $$! 2>/dev/null; do sleep 0.1; done; \
	kill -INT $$! $$(pgrep -P $$!); wait

TopEnvIface:
	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) TopEnvIface -v1 -fno-hide-source-paths -e "a" -e ":q" -fwrite-if-simplified-core -fwrite-interface
	# Second compilation starts from interface files, but still can print "a"
	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) TopEnvIface -v1 -fno-hide-source-paths -e "a" -e ":q" -fwrite-if-simplified-core -fwrite-interface

T27287:
	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) T27287.hs -v1 -fno-hide-source-paths -e ":q" -fwrite-if-simplified-core -fwrite-interface
	# Second compilation starts from interface files, but still can print "a"
	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) T27287.hs -v1 -fno-hide-source-paths -e ":break T27287 5" -e ":q" -fwrite-if-simplified-core -fwrite-interface
