test('different-db',
     [ extra_files(['p/', 'q/', 'r/', 'p1/', 'unitP', 'unitQ', 'unitR', 'unitP1', 'Setup.hs'])
     , js_broken(22349)
     ],
     makefile_test,
     [])
