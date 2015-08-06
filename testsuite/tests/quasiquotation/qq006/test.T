test('qq006',
     [when(fast(), skip),
      reqlib('parsec'),
      extra_clean(['Expr.hi', 'Expr.o']),
      ],
     multimod_compile_fail,
     ['Main', '-v0'])
