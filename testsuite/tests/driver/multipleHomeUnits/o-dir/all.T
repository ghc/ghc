# This test checks that getRootSummary doesn't cross package boundaries.
test('multipleHomeUnits_odir'
    , [ extra_files([ 'p1/', 'unitP1'])
      , req_c
      ]
    , makefile_test
    , ['mhu-odir'])
