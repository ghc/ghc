test('multipleHomeUnits_instance-vis', [extra_files([ 'p1/', 'p2', 'q', 'unitP1', 'unitP2', 'unitQ'])], multiunit_compile, [['unitP1', 'unitP2', 'unitQ'], '-fhide-source-paths'])
