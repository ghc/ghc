test('T13914',
     [ extra_files(['main.hs'])
     , js_broken(22261)
     ],
     makefile_test, ['t13914'])
