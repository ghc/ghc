test('MergeObjsMode',
     [ extra_files(['A.hs', 'B.hs', 'Main.hs'])
     , js_skip # Object merging isn't supported by the JS backend
     ],
     makefile_test,
     [])
