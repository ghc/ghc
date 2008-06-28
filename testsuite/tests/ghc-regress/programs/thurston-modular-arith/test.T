# uses GHC-specific scoped type variables
test('thurston-modular-arith',
     [skip_if_fast,
      extra_clean(['Main.hi', 'Main.o', 'TypeVal.hi', 'TypeVal.o']),
      only_compiler_types(['ghc'])],
     multimod_compile_and_run,
     ['Main', ''])

