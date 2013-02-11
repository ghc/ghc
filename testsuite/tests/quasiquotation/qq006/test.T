test('qq006',
     [when(fast(), skip),
      reqlib('parsec'),
      extra_clean(['Expr.hi', 'Expr.o']),
      only_compiler_types(['ghc'])],
     multimod_compile_fail,
     ['Main', '-v0'])
