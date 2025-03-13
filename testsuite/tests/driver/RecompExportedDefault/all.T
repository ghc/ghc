test('RecompExportedDefault',
     [extra_files(['A.hs','A2.hs']), copy_files
     , expect_broken(25855)],
     makefile_test,
     ['RecompExportedDefault'])
