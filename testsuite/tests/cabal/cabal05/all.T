test('cabal05',
     [extra_files(['Setup.hs', 'p/', 'q/', 'r/', 's/', 't/']),
     js_broken(22349)],
     makefile_test,
     [])
