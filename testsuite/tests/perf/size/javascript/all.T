# These are JavaScript-specific tests based on Google Closure Compiler
setTestOpts(when(not(js_arch()),skip))

test('T24602_perf_size', [collect_size(5, './T24602_perf_size.jsexe/all.min.js')], makefile_test, ['T24602_perf_size'])
