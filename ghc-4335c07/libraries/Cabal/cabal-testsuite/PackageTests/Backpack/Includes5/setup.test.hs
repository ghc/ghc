import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [8,1])
    setup "configure" []
    r <- fails $ setup' "build" []
    assertOutputContains "Foobar" r
    assertOutputContains "Could not find" r
    return ()
