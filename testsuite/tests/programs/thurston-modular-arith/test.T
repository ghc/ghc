# uses GHC-specific scoped type variables
test('thurston-modular-arith',
     [when(fast(), skip),
      extra_clean(['Main.hi', 'Main.o', 'TypeVal.hi', 'TypeVal.o']),
      only_compiler_types(['ghc'])],
     multimod_compile_and_run,
     ['Main', ''])

