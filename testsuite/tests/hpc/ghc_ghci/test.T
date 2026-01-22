
test('hpc_ghc_ghci',
     [extra_files(['A.hs', 'B.hs']),
      only_ways(['normal']), when(compiler_profiled(), skip), req_interp],
     run_command, ['$MAKE -s --no-print-directory hpc_ghc_ghci'])

test('hpc_ghci01',
     [extra_files(['A.hs', 'B.hs']),
      only_ways(['normal']), when(compiler_profiled(), skip), req_interp],
     makefile_test, ['hpc_ghci01 HPC={hpc}'])

test('hpc_ghci02',
     [extra_files(['A.hs', 'B.hs']),
      only_ways(['normal']), when(compiler_profiled(), skip), req_interp],
     makefile_test, ['hpc_ghci02 HPC={hpc}'])

test('hpc_ghc_ghci_bytecode',
     [extra_files(['BytecodeMain.hs']),
      only_ways(['normal']), when(compiler_profiled(), skip), req_interp],
     run_command, ['$MAKE -s --no-print-directory hpc_ghc_ghci_bytecode'])
