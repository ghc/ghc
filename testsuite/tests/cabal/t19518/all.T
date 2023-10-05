test('t19518',
     [ extra_files(['Setup.hs', 'p/', 'q/', 'r/'])
     , js_broken(22349)
     ],
     makefile_test,
     [])
