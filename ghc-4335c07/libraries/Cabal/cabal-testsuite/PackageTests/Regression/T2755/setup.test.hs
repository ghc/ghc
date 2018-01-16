import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    setup "configure" ["--enable-tests"]
    setup "test" []
