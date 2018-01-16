import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [8,1])
    r <- fails $ setup' "configure" ["--cabal-file", "Includes2.cabal.fail"]
    assertOutputContains "mysql" r
