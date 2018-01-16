import Test.Cabal.Prelude
main = setupAndCabalTest . withPackageDb $ do
    withDirectory "to-install" $ setup_install []
    setup_build []
    r <- runExe' "lemon" []
    assertEqual
        ("executable should have linked with the internal library")
        ("foo foo myLibFunc internal")
        (concatOutput (resultOutput r))
