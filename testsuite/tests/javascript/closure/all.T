# These are JavaScript-specific tests based on Google Closure Compiler
setTestOpts(when(not(js_arch()),skip))

test('T24602', normal, makefile_test, ['T24602'])
