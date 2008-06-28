test('qq005',
     [skip_if_fast,
      only_compiler_types(['ghc']),
      extra_clean(['Expr.hi', 'Expr.o', 'Main.hi', 'Main.o']),
      if_compiler_lt('ghc', '6.9', skip)],
	multimod_compile_and_run,
	['Main', ''])
