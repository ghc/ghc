test('qq008',
     [when(fast(), skip),
      extra_clean(['QQ.hi', 'QQ.o', 'Test.hi', 'Test.o']),
      # We'd need to jump through some hoops to run this test the
      # profiling ways, due to the TH use, so for now we just
      # omit the profiling ways
      omit_ways(['profasm','profthreaded']),
      only_compiler_types(['ghc'])],
     multimod_compile,
     ['Test', '-v0'])
