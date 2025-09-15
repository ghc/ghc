test('cabal06',
     [extra_files(['Setup.hs', 'p-1.0/', 'p-1.1/', 'q/', 'r/']),
     js_broken(22349)],
     makefile_test,
     [])
