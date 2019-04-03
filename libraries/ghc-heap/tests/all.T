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

test('heap_weak',
     [when(have_profiling(), extra_ways(['prof'])),
      # These ways produce slightly different heap representations.
      # Currently we don't test them.
      omit_ways(['ghci', 'hpc'])
     ],
     compile_and_run, [''])

# Test everything except FUNs and PAPs in all ways.
test('closure_size',
     [extra_files(['ClosureSizeUtils.hs']),
      when(have_profiling(), extra_ways(['prof'])),
      # These ways produce slightly different heap representations.
      # Currently we don't test them.
      omit_ways(['hpc'])
     ],
     compile_and_run, [''])

# Test PAPs and FUNs only in normal way (e.g. with -O0)
# since otherwise the simplifier interferes.
test('closure_size_noopt',
     [extra_files(['ClosureSizeUtils.hs']),
      only_ways(['normal'])
     ],
     compile_and_run, [''])

