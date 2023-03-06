# These are JavaScript-specific tests
setTestOpts(when(not(js_arch()),skip))

test('T23101', normal, compile_and_run, [''])

test('js-ffi-int', normal, compile_and_run, [''])
test('js-ffi-string', normal, compile_and_run, [''])
test('js-ffi-null', normal, compile_and_run, [''])
test('js-ffi-isNull', normal, compile_and_run, [''])
test('js-ffi-isUndefined', normal, compile_and_run, [''])
test('js-ffi-array', normal, compile_and_run, [''])

test('js-callback01', normal, compile_and_run, [''])
test('js-callback02', normal, compile_and_run, [''])
test('js-callback03', normal, compile_and_run, [''])
test('js-callback04', js_skip, compile_and_run, [''])
test('js-callback05', js_skip, compile_and_run, [''])
