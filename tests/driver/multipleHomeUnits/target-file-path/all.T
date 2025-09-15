# This test checks that getRootSummary doesn't cross package boundaries.
test('multipleHomeUnits_target-file-path'
    , [extra_files([ 'p1/', 'unitP1'])
      ]
    , multiunit_compile
    , [['unitP1'], '-fhide-source-paths'])
