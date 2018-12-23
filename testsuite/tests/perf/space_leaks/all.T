
test('space_leak_001',
     # This could potentially be replaced with
     # collect_stats('all',5) to test all 3 with
     # 5% possible deviation.
     [collect_stats(['peak_megabytes_allocated','bytes allocated'],5),
      collect_stats('max_bytes_used',15),
      omit_ways(['profasm','profthreaded','threaded1','threaded2']),
      reqlib('integer-gmp')
      ],
     compile_and_run,
     [''])

test('T4334',
     # Test for a space leak in Data.List.lines (fixed with #4334)
     [extra_run_opts('1000000 2 t'),
      collect_stats('peak_megabytes_allocated',2),
      # prof ways don't work well with +RTS -V0
      omit_ways(['profasm','profthreaded'])
      ],
     compile_and_run, [''])

test('T2762',
     [collect_stats('peak_megabytes_allocated',2),
      only_ways(['normal']),
      extra_run_opts('+RTS -G1 -RTS' ),
      extra_clean(['T2762A.hi', 'T2762A.o'])],
     compile_and_run, ['-O'])

test('T4018',
     [ only_ways(['optasm']), extra_run_opts('+RTS -M10m -RTS') ],
     compile_and_run, ['-fno-state-hack'])

test('T4029',
     [collect_stats(['peak_megabytes_allocated','max_bytes_used'],10),
      extra_hc_opts('+RTS -G1 -RTS' ),
      ],
     ghci_script,
     ['T4029.script'])

