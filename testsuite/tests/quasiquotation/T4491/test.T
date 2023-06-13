test('T4491',
     [extra_files(['A.hs']),
      
      req_th,
      # We'd need to jump through some hoops to run this test the
      # other ways, due to the TH use, so for now we only run it
      # the TH way
      only_ways([config.ghc_th_way]),
     ],
     compile_and_run, [''])
