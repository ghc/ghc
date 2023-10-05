test('T22333',
     [extra_files(['Setup.hs', 'my-exe', 'my-package-a', 'my-package-b' ]), js_broken(22349)],
     makefile_test,
     [])
