TOP=../../../../../testsuite
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: prep-T19703
prep-T19703:
	"$(PYTHON)" genT19703.py
	"$(TEST_HC)" $(TEST_HC_OPTS) Fam.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) A.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) B.hs
