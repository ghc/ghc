test('ghc_debug_01', [only_ways(['threaded1', 'threaded2']), extra_ways(['threaded1', 'threaded2']),
     extra_files(['ghc_debug.c','ghc_debug.h'])],
     multi_compile_and_run, ['ghc_debug_01', [('ghc_debug.c','')], ''])
test('ghc_debug_02', [only_ways(['threaded1', 'threaded2']), extra_ways(['threaded1', 'threaded2']),
     extra_files(['ghc_debug.c','ghc_debug.h'])],
     multi_compile_and_run, ['ghc_debug_02', [('ghc_debug.c','')], ''])
test('ghc_debug_03', [only_ways(['threaded1', 'threaded2']), extra_ways(['threaded1', 'threaded2']),
     extra_files(['ghc_debug.c','ghc_debug.h'])],
     multi_compile_and_run, ['ghc_debug_03', [('ghc_debug.c','')], ''])
test('ghc_debug_04', [only_ways(['threaded1', 'threaded2']), extra_ways(['threaded1', 'threaded2']),
     extra_files(['ghc_debug.c','ghc_debug.h'])],
     multi_compile_and_run, ['ghc_debug_04', [('ghc_debug.c','')], ''])
