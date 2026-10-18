test('mhu-closure',
     [ extra_files(['p/', 'q/', 'r/', 'r1/', 'unitP', 'unitQ', 'unitR', 'unitR1', 'Setup.hs'])
     , js_broken(22349)
     , filecheck_stdout('mhu-closure.check')
     ],
     makefile_test,
     [])
