# These are JavaScript-specific tests
setTestOpts(when(not(js_arch()),skip))

test('T23101', normal, compile_and_run, [''])
