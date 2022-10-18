test('T13168',
     [extra_files(['package1', 'package2', 'Setup.hs']), js_skip],
     makefile_test, [])
