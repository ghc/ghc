TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T24602_perf_size:
	'$(TEST_HC)' $(TEST_HC_OPTS) ./T24602_perf_size.hs -v0 -fforce-recomp
	google-closure-compiler \
		--platform java \
		--warning_level QUIET \
		--isolation_mode IIFE \
		--assume_function_wrapper \
		--compilation_level ADVANCED_OPTIMIZATIONS \
		--emit_use_strict \
		--js_output_file ./T24602_perf_size.jsexe/all.min.js \
		./T24602_perf_size.jsexe/all.js ./T24602_perf_size.jsexe/all.externs.js
