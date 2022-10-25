
test('typecheck.testeq1', [ extra_files(['FakePrelude.hs', 'Main.hs', 'TypeCast.hs', 'TypeEq.hs'])
                          , when(fast(), skip)
                          , js_broken(22355)
                          ], multimod_compile_and_run,
     ['Main', '-v0'])
