test('T12733',
     [extra_files(['p/', 'q/', 'Setup.hs'])
     , js_broken(22349)
     ],
     makefile_test,
     [])
