test('qq006',
     [skip_if_fast,
      reqlib('parsec'),
      extra_clean(['Expr.hi', 'Expr.o']),
      only_compiler_types(['ghc'])],
     multimod_compile_fail,
     ['Main', '-v0'])
