TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Test that threadDelay can be interrupted by ^C.
T3171:
	echo "do Control.Concurrent.threadDelay 3000000; putStrLn \"threadDelay was not interrupted\"" | \
	"$(TEST_HC)" $(TEST_HC_OPTS) -ignore-dot-ghci -v0 --interactive & \
	sleep 2; kill -INT $$!; wait
