import Test.Cabal.Prelude
-- Internal library used by public library; it must be installed and
-- registered.
main = setupAndCabalTest $
    withPackageDb $ do
        withDirectory "foolib" $ setup_install []
        withDirectory "fooexe" $ do
            setup_build []
            runExe' "fooexe" []
                >>= assertOutputContains "25"
