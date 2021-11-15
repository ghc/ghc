test('T3007',
     [when(opsys("OSJavaScript"), skip), extra_files(['A/', 'B/'])],
     makefile_test, [])
