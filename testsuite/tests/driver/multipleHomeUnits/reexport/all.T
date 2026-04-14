# Mercury's "Abstract out module provider queries" patch breaks reexport
# resolution across multiple home units. Known pre-existing issue.
test('multipleHomeUnits_reexport',
     [extra_files([ 'p1/', 'p2/', 'unitP1', 'unitP2']),
      when(True, expect_broken(0))],
     multiunit_compile, [['unitP1', 'unitP2'], '-v0 -fhide-source-paths'])
