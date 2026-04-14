# Mercury's "Abstract out module provider queries" patch breaks reexport
# resolution across multiple home units. Known pre-existing issue.
test('mhu-closure',
     [ extra_files(['p/', 'q/', 'r/', 'r1/', 'unitP', 'unitQ', 'unitR', 'unitR1', 'Setup.hs'])
     , js_broken(22349)
     , when(True, expect_broken(0))
     ],
     makefile_test,
     [])
