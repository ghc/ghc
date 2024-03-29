setTestOpts(js_skip)

test('space_leak_001',
     # This could potentially be replaced with
     # collect_stats('all',5) to test all 3 with
     # 5% possible deviation.
     [ collect_stats('bytes allocated',5),
      collect_runtime_residency(15),
      omit_ways(['profasm','profthreaded','threaded1','threaded2',
                 'nonmoving_thr', 'nonmoving_thr_sanity'])
      ],
     compile_and_run,
     [''])

test('T4334',
     # Test for a space leak in Data.List.lines (fixed with #4334)
     [extra_run_opts('1000000 2 t'),
      collect_runtime_residency(2),
      # prof ways don't work well with +RTS -V0, nonmoving way residency is
      # highly variable.
      omit_ways(['profasm','profthreaded','nonmoving_thr', 'nonmoving_thr_sanity'])
      ],
     compile_and_run, [''])

test('T2762',
     [collect_runtime_residency(2),
      only_ways(['normal'])],
     compile_and_run, ['-O'])

test('T4018',
     [ only_ways(['optasm']), extra_run_opts('+RTS -M10m -RTS') ],
     compile_and_run, ['-fno-state-hack'])

test('T4029',
     [copy_files, collect_runtime_residency(10),
      ],
     ghci_script,
     ['T4029.script'])

