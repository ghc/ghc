test('bug1465',
     [javascript_failures,
      extra_files(['B1.hs', 'B2.hs', 'C.hs', 'v1/', 'v2/'])],
     makefile_test, [])
