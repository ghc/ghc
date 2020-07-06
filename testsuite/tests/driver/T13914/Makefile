TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

t13914:
	echo "Without -fignore-asserts"
	'$(TEST_HC)' $(TEST_HC_OPTS) main.hs
	('$(TEST_WRAPPER)' ./main 2>&1); true
	sleep 1
	echo "With -fignore-asserts"
	'$(TEST_HC)' $(TEST_HC_OPTS) -fignore-asserts main.hs
	'$(TEST_WRAPPER)' ./main 2>&1
	sleep 1
	echo "Without -fignore-asserts"
	'$(TEST_HC)' $(TEST_HC_OPTS) main.hs
	('$(TEST_WRAPPER)' ./main 2>&1); true
