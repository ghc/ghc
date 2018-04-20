test('qq006', [extra_files(['Expr.hs', 'Main.hs']),
               when(fast(), skip)], multimod_compile_fail,
     ['Main', '-v0'])
