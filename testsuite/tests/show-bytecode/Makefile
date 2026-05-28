TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

compile   = '$(TEST_HC)' $(TEST_HC_OPTS) -fbyte-code -fwrite-byte-code -no-link
show      = '$(TEST_HC)' $(TEST_HC_OPTS) --show-byte-code
normalize = sed -E -e ' \
                s/_r[[:alnum:]]+/_@name_suffix@/g; \
                s/[[:xdigit:]]{32}/@hash@/g; \
                s/word [[:digit:]]{4}[[:digit:]]*/word @large_word@/ \
            '

show-bytecode-vanilla:
	$(compile) Example.hs
	$(show) Example.gbc | $(normalize)

show-bytecode-breakpoints:
	$(compile) -fbreak-points Example.hs
	$(show) Example.gbc | $(normalize)

show-bytecode-hpc:
	$(compile) -fhpc Example.hs
	$(show) Example.gbc | $(normalize)
