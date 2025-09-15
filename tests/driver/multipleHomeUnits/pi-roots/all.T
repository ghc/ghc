# This test checks that getRootSummary doesn't cross package boundaries.
test('multipleHomeUnits_pi_duplicate', [extra_files([ 'p1/', 'p2', 'unitP1', 'unitP2'])], multiunit_compile, [['unitP1', 'unitP2'], '-fhide-source-paths'])
