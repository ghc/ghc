# uses GHC-specific scoped type variables
test('thurston-modular-arith', [extra_files(['Main.hs', 'TypeVal.hs']),
                                when(fast(), skip)], multimod_compile_and_run,
     ['Main', ''])
