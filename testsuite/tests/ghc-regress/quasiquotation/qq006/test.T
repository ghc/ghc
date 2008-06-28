test('qq006',
     [skip_if_fast,
      extra_clean(['Expr.hi', 'Expr.o']),
      only_compiler_types(['ghc']),
      if_compiler_lt('ghc', '6.9', skip)],
     multimod_compile_fail,
     ['Main', '-v0'])
