test('T13615',
     [when(fast(), skip),
      when(unregisterised(), skip),
      req_smp, # needs -N support
      only_ways(threaded_ways),
      extra_files(['Parallel.hs', 'Memo.hs']),
      # Decrease stack chunk size and lots of capabilities to increase failure
      # probability due to more frequent duplicate-computation checks. The
      # reproduction probability is around 75% on my dual-core hyperthreaded
      # laptop.
      extra_run_opts('+RTS -N15 -ki4k')],
     [fragile_for(17269, 'threaded1'), multimod_compile_and_run],
     ['T13615','-rtsopts'])
