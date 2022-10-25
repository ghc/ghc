test('T3007',
     [ extra_files(['A/', 'B/'])
     , js_broken(22356)
     ],
     makefile_test, [])
