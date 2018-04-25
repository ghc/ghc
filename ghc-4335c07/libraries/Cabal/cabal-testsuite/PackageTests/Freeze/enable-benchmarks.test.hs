import Test.Cabal.Prelude
main = cabalTest $ do
    withRepo "repo" . withSourceCopy $ do
        cabal "freeze" ["--enable-benchmarks"]
        cwd <- fmap testCurrentDir getTestEnv
        assertFileDoesContain (cwd </> "cabal.config") "criterion"
        assertFileDoesContain (cwd </> "cabal.config") "ghc-prim"
