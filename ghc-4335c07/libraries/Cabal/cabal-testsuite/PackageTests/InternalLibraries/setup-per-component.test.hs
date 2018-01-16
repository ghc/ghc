import Test.Cabal.Prelude
-- No cabal test because per-component is broken for it
main = setupTest $ do
    withPackageDb $ do
      withDirectory "p" $ do
        setup_install ["q"]
        setup_install ["p"]
        setup_install ["foo"]
        runInstalledExe "foo" []

