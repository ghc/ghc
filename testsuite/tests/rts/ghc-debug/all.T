test('ghc_debug_01',
     [ extra_files(['ghc_debug.c','ghc_debug.h']),
      ignore_stdout,
      ignore_stderr
     ],
     multi_compile_and_run, ['ghc_debug_01', [('ghc_debug.c','')], '-threaded '])
test('ghc_debug_02',
     [ extra_files(['ghc_debug.c','ghc_debug.h']),
      ignore_stdout,
      ignore_stderr
     ],
     multi_compile_and_run, ['ghc_debug_02', [('ghc_debug.c','')], '-threaded '])
test('ghc_debug_03',
     [ extra_files(['ghc_debug.c','ghc_debug.h']),
      ignore_stdout,
      ignore_stderr
     ],
     multi_compile_and_run, ['ghc_debug_03', [('ghc_debug.c','')], '-threaded '])
