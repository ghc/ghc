import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [8,1])
    setup "configure" []
    r <- fails $ setup' "build" []
    assertRegex "error should be in B.hs" "^B.hs:" r
    assertRegex "error should be \"Could not find module Data.Set\""
                "(Could not find module|Failed to load interface).*Data.Set" r
