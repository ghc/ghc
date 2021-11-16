test('apirecomp001',
     [extra_files(['A.hs', 'B.hs', 'myghc.hs']),
      javascript_failures],
     makefile_test, ['apirecomp001'])
