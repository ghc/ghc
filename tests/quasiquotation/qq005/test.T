test('qq005',
     [extra_files(['Expr.hs', 'Main.hs']),
      when(fast(), skip),
      # We'd need to jump through some hoops to run this test the
      # profiling ways, due to the TH use, so for now we just
      # omit the profiling ways
      omit_ways(prof_ways),
      req_interp],
	multimod_compile_and_run,
	['Main', ''])
