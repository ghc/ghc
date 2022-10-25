test('T13168',
     [extra_files(['package1', 'package2', 'Setup.hs']), req_js_compactor],
     makefile_test, [])
