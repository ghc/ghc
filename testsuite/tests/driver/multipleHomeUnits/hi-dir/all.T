# This test checks that getRootSummary doesn't cross package boundaries.
test('multipleHomeUnits_hidir'
    , [extra_files([ 'p1/', 'unitP1'])
      , js_broken(22261)
      ]
    , makefile_test
    , ['mhu-hidir'])
