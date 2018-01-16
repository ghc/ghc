import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [7,9])
    withPackageDb $ do
        withDirectory "containers-dupe" $
            setup_install []
        withDirectory "p" $ do
            r <- fails $ setup' "configure" ["--cabal-file", "p.cabal.fail-ambiguous"]
            assertOutputContains "Data.Map" r
