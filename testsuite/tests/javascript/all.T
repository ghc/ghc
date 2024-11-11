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

test('js-mk_tup', extra_files(['test-mk_tup.js']), compile_and_run, ['test-mk_tup.js'])

test('T23346', normal, compile_and_run, [''])
test('T22455', normal, compile_and_run, ['-ddisable-js-minifier'])
test('T23565', normal, compile_and_run, [''])
test('T24495', normal, makefile_test, ['T24495'])

test('T23479', normal, makefile_test, ['T23479'])
test('T24744', normal, makefile_test, ['T24744'])
