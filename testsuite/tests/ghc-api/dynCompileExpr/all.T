test('dynCompileExpr',
     [ extra_run_opts('"' + config.libdir + '"'),
       omit_ways(prof_ways) ], # cannot run interpreted code with -prof
     compile_and_run, ['-package ghc'])
