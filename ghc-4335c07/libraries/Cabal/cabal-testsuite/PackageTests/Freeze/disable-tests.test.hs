import Test.Cabal.Prelude
main = cabalTest $ do
    withRepo "repo" . withSourceCopy $ do
        cabal "freeze" ["--disable-tests"]
        cwd <- fmap testCurrentDir getTestEnv
        assertFileDoesNotContain (cwd </> "cabal.config") "test-framework"
