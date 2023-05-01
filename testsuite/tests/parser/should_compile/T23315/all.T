test('T23315',
     [extra_files(['Setup.hs']), js_broken(22352)],
     makefile_test, [])
