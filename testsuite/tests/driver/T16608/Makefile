TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T16608_1:
	# FreeBSD's sed doesn't like operating in-place on symlinks. Un-symlinkify.
	mv MyInteger.hs tmp.hs; cp tmp.hs MyInteger.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -O0 T16608_1.hs
	./T16608_1
	sleep 1
	sed -i -e 's/{- . succ -}/. succ/' MyInteger.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -O0 T16608_1.hs
	./T16608_1

T16608_2:
	# FreeBSD's sed doesn't like operating in-place on symlinks. Un-symlinkify.
	mv MyInteger.hs tmp.hs; cp tmp.hs MyInteger.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -O0 T16608_2.hs
	./T16608_2
	sleep 1
	sed -i -e 's/{- . succ -}/. succ/' MyInteger.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -O0 T16608_2.hs
	./T16608_2
