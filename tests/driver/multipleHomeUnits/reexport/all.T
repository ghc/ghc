# This test checks that getRootSummary doesn't cross package boundaries.
test('multipleHomeUnits_reexport', [extra_files([ 'p1/', 'p2/', 'unitP1', 'unitP2'])], multiunit_compile, [['unitP1', 'unitP2'], '-v0 -fhide-source-paths'])
