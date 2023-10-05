setTestOpts([
    unless(opsys('mingw32'), skip),
    extra_files(['T21556_lib.c'])
])
test('T21556a', normal, multimod_compile_and_run, ['T21556a', 'T21556_lib.c'])
test('T21556b', normal, multimod_compile_and_run, ['T21556b', 'T21556_lib.c T21556b_c.c'])
