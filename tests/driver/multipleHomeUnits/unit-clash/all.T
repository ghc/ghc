# This test checks for clashing home unit ids
test('multipleHomeUnits_unit-clash', [extra_files([ 'A.hs', 'B.hs', 'unitA', 'unitB'])], multiunit_compile_fail, [['unitA', 'unitB'], '-fhide-source-paths'])
