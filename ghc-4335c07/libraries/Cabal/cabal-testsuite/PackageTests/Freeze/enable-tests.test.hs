import Test.Cabal.Prelude
main = cabalTest $ do
    withRepo "repo" . withSourceCopy $ do
        cabal "freeze" ["--enable-tests"]
        cwd <- fmap testCurrentDir getTestEnv
        assertFileDoesContain (cwd </> "cabal.config") "test-framework"
