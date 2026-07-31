TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

compile      = '$(TEST_HC)' $(TEST_HC_OPTS) \
                   -fbyte-code -fwrite-byte-code -no-link
show         = '$(TEST_HC)' $(TEST_HC_OPTS) \
                   --show-byte-code
stabilize    = sed -E -e ' \
                   s/_r[[:alnum:]]+/_@name_suffix@/g; \
                   s/^( *hash: )[[:xdigit:]]+/\1@hash@/g; \
                   s/^( *)[[:xdigit:]]+:/\1@hash@:/g; \
                   s/word [[:digit:]]{2}[[:digit:]]*/word @large_word@/ \
               '
universalize = sed -E -e ' \
                   s/UInt[[:digit:]]+/UInt@word_size@/; \
                   s/W[[:digit:]]+#/W@word_size@#/ \
               ' | \
               uniq
normalize    = $(stabilize) | $(universalize)
# The invocation of `uniq` in `$(universalize)` is merely for collapsing
# adjacent entries of `word @large_word@`, whose number may depend on the word
# size.

show-bytecode-vanilla:
	$(compile) Example.hs
	$(show) Example.gbc | $(normalize)

show-bytecode-breakpoints:
	$(compile) -fbreak-points Example.hs
	$(show) Example.gbc | $(normalize)

show-bytecode-hpc:
	$(compile) -fhpc Example.hs
	$(show) Example.gbc | $(normalize)
