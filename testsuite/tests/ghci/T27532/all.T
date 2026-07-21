test('T27532',
     [extra_files(['a.script', 'b.script'])],
     makefile_test, ['T27532'])

test('T27532j4',
     [pre_cmd('./genT27532Modules'),
      extra_files(['genT27532Modules']),
      req_ghc_smp],
     makefile_test, ['T27532j4'])
