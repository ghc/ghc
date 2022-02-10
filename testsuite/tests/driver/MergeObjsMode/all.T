test('MergeObjsMode',
     [ extra_files(['A.hs', 'B.hs', 'Main.hs'])
     , js_broken(22261)
     ],
     makefile_test,
     [])
