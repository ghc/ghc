test('apirecomp001',
     [extra_files(['A.hs', 'B.hs', 'myghc.hs']),
     req_js_compactor],
     makefile_test, ['apirecomp001'])
