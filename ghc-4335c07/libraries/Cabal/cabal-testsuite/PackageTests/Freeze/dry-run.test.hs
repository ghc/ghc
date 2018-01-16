import Test.Cabal.Prelude
main = cabalTest $ do
    withRepo "repo" . withSourceCopy $ do
        recordMode DoNotRecord $ cabal "freeze" ["--dry-run"]
        cwd <- fmap testCurrentDir getTestEnv
        shouldNotExist (cwd </> "cabal.config")
