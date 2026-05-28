TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

compile = '$(TEST_HC)' $(TEST_HC_OPTS) -fbyte-code -fwrite-byte-code -no-link
show    = '$(TEST_HC)' $(TEST_HC_OPTS) --show-byte-code

show-bytecode-vanilla:
	$(compile) Example.hs
	$(show) Example.gbc | ./normalize

show-bytecode-breakpoints:
	$(compile) -fbreak-points Example.hs
	$(show) Example.gbc | ./normalize

show-bytecode-hpc:
	$(compile) -fhpc Example.hs
	$(show) Example.gbc | ./normalize
