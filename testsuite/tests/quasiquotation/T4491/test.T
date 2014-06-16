test('T4491',
     [
      req_interp,
      # We'd need to jump through some hoops to run this test the
      # other ways, due to the TH use, so for now we only run it
      # the TH way
      only_ways([config.ghc_th_way]),
      only_compiler_types(['ghc']),
      extra_clean(['A.hi', 'A.o'])
     ],
     compile_and_run, [''])
