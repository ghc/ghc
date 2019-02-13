test('seward-space-leak',
     [extra_files(['Main.lhs']),
      when(fast(), skip),
      # non-moving GC currently lacks selector optimisation
      omit_ways(['nonmoving', 'nonmoving_thr', 'nonmoving_thr_ghc'])
     ], multimod_compile_and_run,
     ['Main', ''])
