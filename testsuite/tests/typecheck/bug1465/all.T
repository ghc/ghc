test('bug1465',
     [when(opsys('OSJavaScript'), skip),
      extra_files(['B1.hs', 'B2.hs', 'C.hs', 'v1/', 'v2/'])],
     makefile_test, [])
