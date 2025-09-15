# These are JavaScript-specific tests based on Google Closure Compiler
setTestOpts(when(not(js_arch()),skip))

test('T24602_perf_size', [extra_files(['../size_hello_artifact.hs']), collect_size(5, './size_hello_artifact.jsexe/all.min.js')], makefile_test, ['T24602_perf_size'])
test('T25046_perf_size_gzip', [extra_files(['../size_hello_artifact.hs']), collect_size(5, './size_hello_artifact.jsexe/all.min.js.gz')], makefile_test, ['T25046_perf_size_gzip'])
test('T25046_perf_size_unicode', [extra_files(['../size_hello_unicode.hs']), collect_size(5, './size_hello_unicode.jsexe/all.min.js')], makefile_test, ['T25046_perf_size_unicode'])
test('T25046_perf_size_unicode_gzip', [extra_files(['../size_hello_unicode.hs']), collect_size(5, './size_hello_unicode.jsexe/all.min.js.gz')], makefile_test, ['T25046_perf_size_unicode_gzip'])
