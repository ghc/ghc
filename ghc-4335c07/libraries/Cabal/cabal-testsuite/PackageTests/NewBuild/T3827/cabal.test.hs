import Test.Cabal.Prelude
main = cabalTest $ do
    cabal "new-build" ["exe:q"]
