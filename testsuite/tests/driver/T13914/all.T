test('T13914',
     [when(opsys("OSJavaScript"), skip), extra_files(['main.hs'])],
     makefile_test, ['t13914'])
