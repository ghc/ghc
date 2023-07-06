# This test checks that getRootSummary doesn't cross package boundaries.
test('multipleHomeUnits_o-files'
    , [extra_files([ 'p1/', 'unitP1'])
      , pre_cmd('$MAKE -s --no-print-directory setup')]
    , multiunit_compile
    , [['unitP1'], '-fhide-source-paths'])
