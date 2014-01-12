test('qq005',
     [when(fast(), skip),
      reqlib('parsec'),
      only_compiler_types(['ghc']),
      # We'd need to jump through some hoops to run this test the
      # profiling ways, due to the TH use, so for now we just
      # omit the profiling ways
      omit_ways(['profasm','profthreaded']),
      extra_clean(['Expr.hi', 'Expr.o', 'Main.hi', 'Main.o'])],
	multimod_compile_and_run,
	['Main', ''])
