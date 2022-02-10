test('T13168',
     [extra_files(['package1', 'package2', 'Setup.hs']), js_broken(22352)],
     makefile_test, [])
