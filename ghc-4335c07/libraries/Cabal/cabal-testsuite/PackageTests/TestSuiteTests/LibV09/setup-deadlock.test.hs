import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnless =<< hasCabalForGhc
    setup_build ["--enable-tests"]
    fails $ setup "test" []
