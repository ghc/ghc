test('qq005',
     [skip_if_fast,
      reqlib('parsec'),
      only_compiler_types(['ghc']),
      # We'd need to jump through some hoops to run this test the
      # profiling ways, due to the TH use, so for now we just
      # omit the profiling ways
      omit_ways(['profc','profasm','profthreaded']),
      # Omit the hpc way for 6.10 due to #1779
      # Unfortunately this requires repeating the prof ways again
      if_compiler_lt('ghc', '6.11',
                     omit_ways(['hpc','profc','profasm','profthreaded'])),
      extra_clean(['Expr.hi', 'Expr.o', 'Main.hi', 'Main.o'])],
	multimod_compile_and_run,
	['Main', ''])
