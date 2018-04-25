import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [8,1])
    r <- fails $ setup' "configure" []
    assertOutputContains "UnfilledSig" r
    return ()
