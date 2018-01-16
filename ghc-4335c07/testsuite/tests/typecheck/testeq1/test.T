
test('typecheck.testeq1', [extra_files(['FakePrelude.hs', 'Main.hs', 'TypeCast.hs', 'TypeEq.hs']),
                           when(fast(), skip)], multimod_compile_and_run,
     ['Main', '-v0'])
