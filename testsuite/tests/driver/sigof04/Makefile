TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -rf containers

sigof04:
	'$(GHC_PKG)' field containers key | sed 's/^.*: *//' > containers
	! '$(TEST_HC)' $(TEST_HC_OPTS)  -c Sig.hsig -sig-of "Sig is `cat containers`:Data.Map.Strict"
