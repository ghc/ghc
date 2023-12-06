
test('typecheck.testeq1', [ extra_files(['FakePrelude.hs', 'Main.hs', 'TypeCast.hs', 'TypeEq.hs'])
                          , when(fast(), skip)
                          , ignore_extension
                          , js_broken(22355)
                          # https://gitlab.haskell.org/ghc/ghc/-/issues/23238
                          , when(arch('wasm32'), skip)
                          ], multimod_compile_and_run,
     ['Main', '-v0'])
