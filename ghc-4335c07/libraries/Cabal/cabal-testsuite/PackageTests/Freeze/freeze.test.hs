import Test.Cabal.Prelude
main = cabalTest $ do
    withRepo "repo" . withSourceCopy $ do
        -- TODO: test this with a sandbox-installed package
        -- that is not depended upon
        cabal "freeze" []
        cwd <- fmap testCurrentDir getTestEnv
        assertFileDoesNotContain (cwd </> "cabal.config") "exceptions"
        assertFileDoesNotContain (cwd </> "cabal.config") "my"
        assertFileDoesContain (cwd </> "cabal.config") "base"
