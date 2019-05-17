test('heap_all',
     [when(have_profiling(), extra_ways(['prof'])),
      # These ways produce slightly different heap representations.
      # Currently we don't test them.
      omit_ways(['ghci', 'hpc',
                 'nonmoving', 'nonmoving_thr', 'nonmoving_thr_ghc']),
      # The debug RTS initializes some fields with 0xaa and so
      # this test spuriously fails.
      when(compiler_debugged(), skip)
     ],
     compile_and_run, [''])
