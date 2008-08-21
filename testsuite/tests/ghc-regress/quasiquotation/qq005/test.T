test('qq005',
     [skip_if_fast,
      reqlib('parsec'),
      only_compiler_types(['ghc']),
      extra_clean(['Expr.hi', 'Expr.o', 'Main.hi', 'Main.o'])],
	multimod_compile_and_run,
	['Main', ''])
