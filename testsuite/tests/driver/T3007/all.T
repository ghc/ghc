test('T3007',
     [ extra_files(['A/', 'B/'])
     , js_broken(22349)
     ],
     makefile_test, [])
