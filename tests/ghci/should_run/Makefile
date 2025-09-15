TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Test that threadDelay can be interrupted by ^C.
T3171:
	echo "do Control.Concurrent.threadDelay 3000000; putStrLn \"threadDelay was not interrupted\"" | \
	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) & \
	sleep 2; kill -INT $$!; wait

TopEnvIface:
	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) TopEnvIface -v1 -e "a" -e ":q" -fwrite-if-simplified-core -fwrite-interface
	# Second compilation starts from interface files, but still can print "a"
	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) TopEnvIface -v1 -e "a" -e ":q" -fwrite-if-simplified-core -fwrite-interface

