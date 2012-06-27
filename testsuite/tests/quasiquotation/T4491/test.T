test('T4491',
     [
      req_interp,
      # We'd need to jump through some hoops to run this test the
      # in the prof or dyn ways, due to the TH use, so for now we just
      # omit the other ways
      omit_ways(['profasm','profthreaded','dyn']),
      only_compiler_types(['ghc']),
      extra_clean(['A.hi', 'A.o'])
     ],
     compile_and_run, [''])
