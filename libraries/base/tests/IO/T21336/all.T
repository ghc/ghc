# N.B. /dev/full exists on Darwin but cannot be opened, failing with -EPERM
test('T21336a',
     [ unless(opsys('linux') or opsys('freebsd'), skip)
     , js_broken(22261)
     , fragile(22022)
     # More than fragile, the test is failing consistently on wasm. See #22022.
     # It would be nice to see if the test is NOT fragile on the other
     # architectures, but right now I don't know how to check.
     , when(arch('wasm32'), skip)
     , extra_files(['FinalizerExceptionHandler.hs'])
     ],
     compile_and_run, [''])
test('T21336b',
     [ unless(opsys('linux') or opsys('freebsd'), skip)
     , js_broken(22352)
     , extra_files(['FinalizerExceptionHandler.hs'])
     ],
     makefile_test, [])
test('T21336c',
     [ unless(opsys('linux') or opsys('freebsd'), skip)
     , js_broken(22370)
     , extra_files(['FinalizerExceptionHandler.hs'])
     ],
     makefile_test, [])
