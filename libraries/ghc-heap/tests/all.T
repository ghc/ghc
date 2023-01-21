setTestOpts(js_skip) # ghc-heap not supported by the JS backend

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

test('tso_and_stack_closures',
     [extra_files(['create_tso.c','create_tso.h','TestUtils.hs','stack_lib.c']),
      only_ways(['profthreaded']),
      ignore_stdout,
      ignore_stderr
     ],
     multi_compile_and_run, ['tso_and_stack_closures', [('create_tso.c',''), ('stack_lib.c', '')], ''])

test('parse_tso_flags',
     [extra_files(['stack_lib.c', 'TestUtils.hs']),
      only_ways(['normal']),
      ignore_stdout,
      ignore_stderr
     ],
     multi_compile_and_run, ['parse_tso_flags', [('stack_lib.c','')], ''])
test('T21622',
     only_ways(['normal']),
     compile_and_run, [''])

# TODO: Remove debug flags
test('stack_big_ret',
     [
        extra_files(['stack_lib.c', 'TestUtils.hs']),
        ignore_stdout,
        ignore_stderr
     ],
     multi_compile_and_run,
     ['stack_big_ret', [('stack_lib.c','')], '-debug -optc-g -g'])

# TODO: Remove debug flags
# Options:
#   - `-kc512B -kb64B`: Make stack chunk size small to provoke underflow stack frames.
test('stack_underflow',
     [
        extra_files(['stack_lib.c', 'TestUtils.hs']),
        extra_run_opts('+RTS -kc512B -kb64B -RTS'),
        ignore_stdout,
        ignore_stderr
     ],
     multi_compile_and_run,
     ['stack_underflow', [('stack_lib.c','')], '-debug -optc-g -g'])

# TODO: Remove debug flags
test('stack_stm_frames',
     [
        extra_files(['stack_lib.c', 'TestUtils.hs']),
        ignore_stdout,
        ignore_stderr
      ],
     multi_compile_and_run,
     ['stack_stm_frames', [('stack_lib.c','')], '-debug -optc-g -g'])

# TODO: Remove debug flags
test('stack_misc_closures',
     [
        extra_files(['stack_misc_closures_c.c', 'stack_misc_closures_prim.cmm','stack_lib.c', 'TestUtils.hs']),
        ignore_stdout,
        ignore_stderr
      ],
     multi_compile_and_run,
     ['stack_misc_closures',
        [ ('stack_misc_closures_c.c', '')
         ,('stack_misc_closures_prim.cmm', '')
         ,('stack_lib.c', '')
         ]
      , '-debug -optc-g -g -ddump-to-file -dlint -dppr-debug -ddump-cmm'
      ])
