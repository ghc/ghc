test('MergeObjsMode',
     [ extra_files(['A.hs', 'B.hs', 'Main.hs'])
     , js_skip
     ],
     makefile_test,
     [])
