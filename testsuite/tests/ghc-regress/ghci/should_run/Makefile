TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Test that threadDelay can be interrupted by ^C.
3171:
	(sleep 1; kill -INT `cat 3171.pid`) & \
	  echo "do System.Posix.getProcessID >>= writeFile \"3171.pid\" . show; Control.Concurrent.threadDelay 3000000; putStrLn \"threadDelay was not interrupted\"" | $(TEST_HC) -v0 --interactive 2>3171.err
