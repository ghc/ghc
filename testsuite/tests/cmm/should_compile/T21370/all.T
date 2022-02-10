test('T21370',
  [ extra_files(["subdir", "test.cmm", "test2.cmm", "Main.hs"])
  , js_skip # use Cmm
  ], makefile_test, [])
