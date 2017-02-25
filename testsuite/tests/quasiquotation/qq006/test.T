test('qq006', [extra_files(['Expr.hs', 'Main.hs']),
               when(fast(), skip), reqlib('parsec')], multimod_compile_fail,
     ['Main', '-v0'])
