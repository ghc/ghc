TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T24602_perf_size:
	'$(TEST_HC)' $(TEST_HC_OPTS) ./size_hello_artifact.hs -v0 -fforce-recomp
	google-closure-compiler \
		--platform java \
		--warning_level QUIET \
		--isolation_mode IIFE \
		--assume_function_wrapper \
		--compilation_level ADVANCED_OPTIMIZATIONS \
		--emit_use_strict \
		--js_output_file ./size_hello_artifact.jsexe/all.min.js \
		./size_hello_artifact.jsexe/all.js ./size_hello_artifact.jsexe/all.externs.js

T25046_perf_size_gzip: T24602_perf_size
	gzip --best ./size_hello_artifact.jsexe/all.min.js

T25046_perf_size_unicode:
	'$(TEST_HC)' $(TEST_HC_OPTS) ./size_hello_unicode.hs -v0 -fforce-recomp
	google-closure-compiler \
		--platform java \
		--warning_level QUIET \
		--isolation_mode IIFE \
		--assume_function_wrapper \
		--compilation_level ADVANCED_OPTIMIZATIONS \
		--emit_use_strict \
		--js_output_file ./size_hello_unicode.jsexe/all.min.js \
		./size_hello_unicode.jsexe/all.js ./size_hello_unicode.jsexe/all.externs.js

T25046_perf_size_unicode_gzip: T25046_perf_size_unicode
	gzip --best ./size_hello_unicode.jsexe/all.min.js
