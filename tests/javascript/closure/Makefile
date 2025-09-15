TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T24602:
	'$(TEST_HC)' $(TEST_HC_OPTS) T24602.hs -v0 -fforce-recomp
	google-closure-compiler \
		--platform java \
		--warning_level QUIET \
		--isolation_mode IIFE \
		--assume_function_wrapper \
		--compilation_level ADVANCED_OPTIMIZATIONS \
		--emit_use_strict \
		--js_output_file ./T24602.jsexe/all.min.js \
		./T24602.jsexe/all.js ./T24602.jsexe/all.externs.js
	node ./T24602.jsexe/all.min.js
