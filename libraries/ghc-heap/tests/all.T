test('heap_all',
     [when(have_profiling(),
      extra_ways(['prof'])),
      # These ways produce slightly different heap representations.
      # Currently we don't test them.
      omit_ways(['ghci', 'hpc'])
     ],
     compile_and_run, [''])
