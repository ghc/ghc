TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T9074
T9074 :
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -c -fPIC T9074b.c
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -c -fPIC T9074a.c
	# Historically, loading T9074b.o before T9074a.o could fail with
	# "Loading temp shared object failed ... undefined symbol: shared_value".
	echo "main" | "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) \
	    -v0 T9074b.o T9074a.o T9074.hs
