test('dynCompileExpr',
     [ extra_run_opts('"' + config.libdir + '"'),
       when(opsys('mingw32'), expect_broken_for(5987, ['dyn'])),
       unless(config.have_RTS_linker, skip),
       omit_ways(prof_ways) ], # cannot run interpreted code with -prof
     compile_and_run, ['-package ghc'])
