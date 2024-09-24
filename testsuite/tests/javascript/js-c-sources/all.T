# These are JavaScript-specific tests
setTestOpts(when(not(js_arch()),skip))

test('js-c-sources01', extra_files(['js-c-sources01_c.c','js-c-sources01_js.js']), compile_and_run, ['js-c-sources01_c.c js-c-sources01_js.js'])
test('js-c-sources02', extra_files(['js-c-sources02_c.c','js-c-sources02_js.js']), compile_and_run, ['js-c-sources02_c.c js-c-sources02_js.js'])
