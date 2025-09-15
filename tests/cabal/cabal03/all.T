test('cabal03',
     [extra_files(['Setup.lhs', 'p/', 'q/', 'r/']),
     js_broken(22349)],
     makefile_test,
     [])
