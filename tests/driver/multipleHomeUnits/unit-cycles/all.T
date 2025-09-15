# This test checks that cycles between units are not allowed.
test('multipleHomeUnits_unit-cycles', [extra_files([ 'p1/', 'p2/', 'unitP1', 'unitP2'])], multiunit_compile_fail, [['unitP1', 'unitP2'], '-fhide-source-paths'])
