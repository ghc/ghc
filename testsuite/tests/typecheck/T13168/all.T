test('T13168',
     [extra_files(['package1', 'package2', 'Setup.hs']), js_broken(22349)],
     makefile_test, [])
