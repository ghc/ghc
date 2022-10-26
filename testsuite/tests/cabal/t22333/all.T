test('T22333',
     [extra_files(['Setup.hs', 'my-exe', 'my-package-a', 'my-package-b' ])],
     makefile_test,
     [])
