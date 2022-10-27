test('apirecomp001',
     [extra_files(['A.hs', 'B.hs', 'myghc.hs']),
     js_broken(22352)],
     makefile_test, ['apirecomp001'])
