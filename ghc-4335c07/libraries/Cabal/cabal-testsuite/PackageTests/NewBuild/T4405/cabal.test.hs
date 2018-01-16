import Test.Cabal.Prelude
main = cabalTest $ do
    cabal "new-build" ["q"]
    cabal "new-build" ["q"]
