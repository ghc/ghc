test('T27532j4',
     [pre_cmd('./genT27532Modules'),
      extra_files(['genT27532Modules']),
      req_interp,
      req_ghc_smp],
     makefile_test, ['T27532j4'])
