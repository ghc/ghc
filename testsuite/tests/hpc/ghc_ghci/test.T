
test('hpc_ghc_ghci',
     [extra_files(['A.hs', 'B.hs']),
      only_ways(['normal']), when(compiler_profiled(), skip), req_interp],
     run_command, ['$MAKE -s --no-print-directory hpc_ghc_ghci'])
