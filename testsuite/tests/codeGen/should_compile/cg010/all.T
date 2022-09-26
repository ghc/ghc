test('cg010',
  [ extra_files(['A.hs','Main.hs'])
  , js_skip # skip with JS backend because Cmm is required
  ], makefile_test, ['cg010'])
