test(
    'show-bytecode-vanilla',
    extra_files(['Example.hs', 'normalize']),
    makefile_test,
    []
)
test(
    'show-bytecode-breakpoints',
    extra_files(['Example.hs', 'normalize']),
    makefile_test,
    []
)
test(
    'show-bytecode-hpc',
    [js_skip, extra_files(['Example.hs', 'normalize'])],
    makefile_test,
    []
)
