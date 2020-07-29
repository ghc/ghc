test('expandAtFile',
    [extra_files(['expandAtFile/foo', 'expandAtFile/bar', 'expandAtFile/baz'])],
    compile_and_run, [''])
