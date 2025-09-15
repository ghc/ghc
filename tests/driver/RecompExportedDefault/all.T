test('RecompExportedDefault',
     [extra_files(['A.hs','A2.hs', 'A3.hs', 'A4.hs']), copy_files
     ],
     makefile_test,
     ['RecompExportedDefault'])
