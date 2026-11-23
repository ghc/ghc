setTestOpts([omit_ways(ghci_ways + ['threaded2']), when(fast(), skip), js_skip])

hpc_prefix = "perl hpcrun.pl --clear --exeext={exeext} --hpc={hpc}"

test('hpc_fork',
     [extra_files(['../hpcrun.pl']),
      when(opsys('mingw32'), skip), # no forkProcess on Windows
      # profiling and forkProcess don't go well together. See #16907, #11645,
      # and #8862.
      req_process,
      omit_ways(prof_ways),
      cmd_prefix(hpc_prefix),
      # The threaded nonmoving-gc doesn't support forkProcess. See #27547
      omit_ways(['nonmoving_thr', 'nonmoving_thr_ghc', 'nonmoving_thr_sanity'])],
     compile_and_run, ['-fhpc'])
