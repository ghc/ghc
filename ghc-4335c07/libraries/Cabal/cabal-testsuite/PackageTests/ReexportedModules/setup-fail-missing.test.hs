import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [7,9])
    withDirectory "p" $ do
        r <- fails $ setup' "configure" ["--cabal-file", "p.cabal.fail-missing"]
        assertOutputContains "Missing" r
