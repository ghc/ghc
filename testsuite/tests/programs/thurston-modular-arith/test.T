# uses GHC-specific scoped type variables
test('thurston-modular-arith', [when(fast(), skip)], multimod_compile_and_run,
     ['Main', ''])
