test('apirecomp001',
     [extra_files(['A.hs', 'B.hs', 'myghc.hs']),
     js_skip],
     makefile_test, ['apirecomp001'])
