# N.B. /dev/full exists on Darwin but cannot be opened, failing with -EPERM
test('T21336a',
     [ unless(opsys('linux') or opsys('freebsd'), skip)
     , js_broken(22261)
     , fragile(22022)
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
